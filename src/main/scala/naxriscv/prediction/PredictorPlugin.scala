package naxriscv.prediction

import naxriscv.Fetch._
import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK}
import naxriscv.Global._
import naxriscv.prediction.Prediction._
import naxriscv.fetch.{AlignerPlugin, FetchConditionalPrediction, FetchPlugin, PcPlugin}
import naxriscv.frontend.{DecoderPlugin, FrontendPlugin}
import naxriscv.interfaces.{CommitService, JumpService, RobService}
import naxriscv.riscv.{IMM, Rvi}
import naxriscv.utilities.Plugin
import naxriscv.{Frontend, Global, ROB}
import spinal.core._
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.pipeline.{Stageable, StageableOffset}

class PredictorPlugin() extends Plugin{
  val keys = create early new AreaRoot{
    val BRANCH_CONDITIONAL = Stageable(Bool())
    val BRANCH_HISTORY_WIDTH = 16 //TODO
    val BRANCH_HISTORY = Stageable(Bits(BRANCH_HISTORY_WIDTH bits))
  }

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val jump = getService[JumpService]
    val rob = getService[RobService]
    val aligner = getService[AlignerPlugin]
    val branchContext = getService[BranchContextPlugin]
    val decodeJump = jump.createJumpInterface(JumpService.Priorities.DECODE_PREDICTION)
    frontend.retain()
    fetch.retain()
    rob.retain()
    branchContext.retain()

    aligner.addWordContext(
      CONDITIONAL_TAKE_IT,
      keys.BRANCH_HISTORY
    )
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val decoder = getService[DecoderPlugin]
    val rob = getService[RobService]
    val commit = getService[CommitService]
    val branchContext = getService[BranchContextPlugin]

    val branchHistory = new Area{
      val fetchInsertAt = getServicesOf[FetchConditionalPrediction].map(_.useHistoryAt).min
      val onCommit = new Area {
        val value = Reg(keys.BRANCH_HISTORY) init (0)

        val event = commit.onCommit()
        val isConditionalBranch = rob.readAsync(keys.BRANCH_CONDITIONAL, Global.COMMIT_COUNT, event.robId)
        val isTaken = rob.readAsync(branchContext.keys.BRANCH_TAKEN, Global.COMMIT_COUNT, event.robId)
        var valueNext = CombInit(value)
        for (slotId <- 0 until Global.COMMIT_COUNT) {
          when(event.mask(slotId) && isConditionalBranch(slotId)) {
            valueNext \= valueNext.dropHigh(1) ## isTaken(slotId)
          }
        }
        value := valueNext.resized
      }

      val onDecode = new Area{
        val value = Reg(keys.BRANCH_HISTORY) init (0)
      }

      val onFetch = new Area{
        val value = Reg(keys.BRANCH_HISTORY) init (0)
      }
    }

    val ras = new Area{
      val rasDepth = 32
      val mem = new Area{
        val stack = Mem.fill(rasDepth)(PC)
      }
      val ptr = new Area{
        val push = Reg(UInt(log2Up(rasDepth) bits)) init(0)
        val pop = Reg(UInt(log2Up(rasDepth) bits)) init(rasDepth-1)
        val pushIt, popIt = False

        push := push + U(pushIt) - U(popIt)
        pop  := pop + U(pushIt) - U(popIt)
      }
      val read = mem.stack.readAsync(ptr.pop)
      val write = mem.stack.writePort
      write.valid := ptr.pushIt
      write.address := ptr.push
      write.data.assignDontCare()
    }


    val decodePatch = new Area {
      val stage = frontend.pipeline.decoded
      val stagePrevious = frontend.pipeline.aligned
      val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
      val branchDecoder, jalDecoder, jalrDecoder, anyDecoder = new DecodingSpec(Bool()).setDefault(Masked.zero)
      branchDecoder.addNeeds(branchKeys, Masked.one)
      jalDecoder.addNeeds(Masked(Rvi.JAL.key), Masked.one)
      jalrDecoder.addNeeds(Masked(Rvi.JALR.key), Masked.one)
      anyDecoder.addNeeds(branchKeys ++ List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key)), Masked.one)

      import stage._

      var rasPushUsed = False
      var rasPopUsed = False
      val slots = for (slotId <- 0 until Frontend.DECODE_COUNT) yield new Area {
        implicit val _ = StageableOffset(slotId)

        def inst = Frontend.INSTRUCTION_DECOMPRESSED

        val isJal = jalDecoder.build(inst, decoder.covers())
        val isJalR = jalrDecoder.build(inst, decoder.covers())
        val isBranch = branchDecoder.build(inst, decoder.covers())
        val isAny = anyDecoder.build(inst, decoder.covers())
        val rdLink  = List(1,5).map(decoder.ARCH_RD === _).orR
        val rs1Link = List(1,5).map(decoder.ARCH_RS(0) === _).orR
        val rdEquRs1 = decoder.ARCH_RD === decoder.ARCH_RS(0)
        val rasPush = (isJal || isJalR) && rdLink
        val rasPop  = isJalR && (!rdLink && rs1Link || rdLink && rs1Link && !rdEquRs1)

        val imm = IMM(inst)
        val offset = Frontend.INSTRUCTION_DECOMPRESSED(2).mux(
          False -> imm.b_sext,
          True -> imm.j_sext
        )

        val lastSlice = PC(SLICE_RANGE) + INSTRUCTION_SLICE_COUNT
        val conditionalPrediction =  CONDITIONAL_TAKE_IT(lastSlice)//TODO pipeline it from earlier stage

        val slices = INSTRUCTION_SLICE_COUNT +^ 1
        val pcInc = S(PC + (slices << SLICE_RANGE_LOW))
        val pcTarget = S(PC) + offset
        when(isJalR){ pcTarget := S(ras.read) }
        val canImprove = !isJalR || rasPop
        val branchedPrediction = isBranch && conditionalPrediction || isJal || isJalR
        val pcPrediction = branchedPrediction ? pcTarget otherwise pcInc
        val pcNext = canImprove ?  U(pcPrediction) otherwise ALIGNED_BRANCH_PC_NEXT
        val missmatch = !ALIGNED_BRANCH_VALID && branchedPrediction || ALIGNED_BRANCH_VALID && ALIGNED_BRANCH_PC_NEXT =/= U(pcPrediction)
        val needCorrection = Frontend.DISPATCH_MASK && canImprove && missmatch

        branchContext.keys.BRANCH_SEL := isAny
        branchContext.keys.BRANCH_EARLY.pcNext := pcNext
        keys.BRANCH_CONDITIONAL := isBranch

        when(stage.resulting(DISPATCH_MASK, slotId) && rasPush) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
          when(!rasPushUsed){
            ras.write.data := U(pcInc)
          }
          rasPushUsed \= True
        }
        when(stage.resulting(DISPATCH_MASK, slotId) && rasPop) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
          rasPopUsed \= True
        }
      }
      when(isFireing){
        ras.ptr.pushIt    setWhen(rasPushUsed)
        ras.ptr.popIt     setWhen(rasPopUsed)
      }

      val hit = slots.map(_.needCorrection).orR
      val selOh = OHMasking.first(slots.map(_.needCorrection))
      setup.decodeJump.valid := isFireing && hit
      setup.decodeJump.pc := U(MuxOH(selOh, slots.map(_.pcPrediction)))

      flushIt(setup.decodeJump.valid, root = false)

      //WARNING, overloaded(Frontend.DISPATCH_MASK) may not be reconized by some downstream plugins if you move this futher the decoding stage
      for (slotId <- 1 until Frontend.DECODE_COUNT) {
        stage.overloaded(Frontend.DISPATCH_MASK, slotId) := stage(Frontend.DISPATCH_MASK, slotId)&& !(0 until slotId).map(i => slots(i).needCorrection).orR
      }

      var branchHistoryNext  = CombInit(branchHistory.onDecode.value)
      for (slotId <- 0 until Frontend.DECODE_COUNT) {
        val slot = slots(slotId)
        when(slot.isBranch && (Frontend.DISPATCH_MASK, slotId) && B(slots.take(slotId).map(s => s.needCorrection)) === 0){
          branchHistoryNext \= branchHistoryNext.dropHigh(1) ## slot.branchedPrediction
        }
      }
      when(isFireing){
        branchHistory.onDecode.value := branchHistoryNext
        branchHistory.onFetch.value := branchHistoryNext //TODO REMOVE ME (DEBUG), instead the update of the onFetch history should be done by gshare itself, in the fetch stages, to provide a more responsive branch history
      }
    }

    val update = new Area{
      val stage = frontend.pipeline.dispatch
      import stage._
      rob.write(keys.BRANCH_CONDITIONAL, DISPATCH_COUNT, (0 until DISPATCH_COUNT).map(stage(keys.BRANCH_CONDITIONAL, _)),  ROB.ID, isFireing)

      branchContext.dispatchWrite(
        keys.BRANCH_HISTORY,
        keys.BRANCH_CONDITIONAL,
        CONDITIONAL_TAKE_IT
      )
    }

    val branchHistoryUpdates = new Area{
      import branchHistory._

      assert(fetchInsertAt >= 1, "Would require some bypass of the stage(0) value, maybe later it could be implemented")
      fetch.getStage(fetchInsertAt)(keys.BRANCH_HISTORY) := onFetch.value

      val fetchJumps = getService[PcPlugin].getFetchJumps()
      val grouped = fetchJumps.groupBy(_._1)
      val ordered = grouped.toSeq.sortBy(_._1).map(_._2)
      val group = for(group <- grouped) yield new Area{ //TODO manage case where the group is < than branchHistoryFetchAt
        val valid = group._2.map(_._2).orR
        when(valid){
          onFetch.value := fetch.getStage(group._1).resulting(keys.BRANCH_HISTORY)
        }
      }
      when(setup.decodeJump.valid){
        onFetch.value := decodePatch.branchHistoryNext
      }
      when(commit.reschedulingPort().valid){
        onFetch.value := onCommit.valueNext
        onDecode.value := onCommit.valueNext
      }
    }
    rob.release()
    frontend.release()
    fetch.release()
    branchContext.release()
  }
}