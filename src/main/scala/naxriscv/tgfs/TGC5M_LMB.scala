package naxriscv.tgfs

import spinal.core._
import naxriscv.compatibility._
import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.frontend._
import naxriscv.fetch._
import naxriscv.misc._
import naxriscv.execute._
import naxriscv.execute.fpu._
import naxriscv.lsu._
import naxriscv.lsu2.Lsu2Plugin
import naxriscv.prediction._
import naxriscv.riscv
import naxriscv.riscv.IntRegFile
import naxriscv.utilities._
import spinal.lib.cpu.riscv.debug.DebugTransportModuleParameter
import spinal.lib.eda.bench.Rtl

import scala.collection.mutable.ArrayBuffer


class NaxRiscv(val plugins : Seq[Plugin]) extends Component{
  val database = new DataBase
  val framework = NaxScope(database) on new Framework(plugins) //Will run the generation asynchronously
}

object Config{
  def plugins(resetVector : BigInt = 0x80000000l,
              withRdTime : Boolean = true,
              ioRange    : UInt => Bool = _(31 downto 28) === 0x1,
              fetchRange : UInt => Bool = _(31 downto 28) =/= 0x1,
              aluCount : Int = 2,
              decodeCount : Int = 2,
              withRvc : Boolean = false,
              withMmu : Boolean = true,
              withPerfCounters : Boolean = true,
              withSupervisor : Boolean = true,
              withDistributedRam : Boolean = true,
              xlen : Int = 32,
              withLoadStore : Boolean = true,
              withDedicatedLoadAgu : Boolean = false,
              withDebug : Boolean = false,
              withEmbeddedJtagTap : Boolean = false,
              withEmbeddedJtagInstruction : Boolean = false,
              jtagTunneled : Boolean = false,
              debugTriggers : Int = 0,
              branchCount : Int = 16,
              withFloat  : Boolean = false,
              withDouble : Boolean = false,
              withLsu2 : Boolean = true,
              lqSize : Int = 16,
              sqSize : Int = 16,
              simulation : Boolean = GenerationFlags.simulation,
              sideChannels : Boolean = false,
              dispatchSlots : Int = 32,
              robSize : Int = 64): ArrayBuffer[Plugin] ={
    val plugins = ArrayBuffer[Plugin]()

    val fpu = withFloat || withDouble

    plugins += new DocPlugin()
    plugins += (withMmu match {
      case false => new StaticAddressTranslationPlugin(
        ioRange = ioRange,
        fetchRange = fetchRange,
        physicalWidth = 32
      )
      case true => new MmuPlugin(
        spec    = if(xlen == 32) MmuSpec.sv32 else MmuSpec.sv39,
        ioRange = ioRange,
        fetchRange = fetchRange,
        physicalWidth = 32
      )
    })
    if(withEmbeddedJtagTap || withEmbeddedJtagInstruction) plugins += new EmbeddedJtagPlugin(
      p = DebugTransportModuleParameter(
        addressWidth = 7,
        version      = 1,
        idle         = 7
      ),
      withTunneling = jtagTunneled,
      withTap = withEmbeddedJtagTap
    )

    //FETCH
    plugins += new FetchPlugin()
    plugins += new PcPlugin(resetVector)
    plugins += new FetchCachePlugin(
      cacheSize = 4096*4,
      wayCount = 4,
      injectionAt = 2,
      fetchDataWidth = 64,
      memDataWidth = 64,
      reducedBankWidth = false,
      hitsWithTranslationWays = true,
      tagsReadAsync = withDistributedRam,
      translationStorageParameter = MmuStorageParameter(
        levels   = List(
          MmuStorageLevel(
            id    = 0,
            ways  = 4,
            depth = 32
          ),
          MmuStorageLevel(
            id    = 1,
            ways  = 2,
            depth = 32
          )
        ),
        priority = 0
      ),
      translationPortParameter = withMmu match {
        case false => StaticAddressTranslationParameter(rspAt = 1)
        case true => MmuPortParameter(
          readAt = 1,
          hitsAt = 1,
          ctrlAt = 1,
          rspAt = 1
        )
      }
    )
    plugins += new AlignerPlugin(
      decodeCount = decodeCount,
      inputAt = 2
    )

    //FRONTEND
    plugins += new FrontendPlugin()
    plugins += new DecompressorPlugin(
      enabled = withRvc,
      pipelined = withRvc
    )
    plugins += new DecoderPlugin(xlen)
    plugins += new RfTranslationPlugin(riscv.IntRegFile)
    plugins += new RfDependencyPlugin()
    plugins += new RfAllocationPlugin(riscv.IntRegFile)
    plugins += new DispatchPlugin(
      slotCount = dispatchSlots,
      robIdAt = withDistributedRam.toInt //Not having it enabled allows ram block inferation on execution unit context reads
    )

    //BRANCH PREDICTION
    plugins += new BranchContextPlugin(
      branchCount = branchCount
    )
    plugins += new HistoryPlugin(
      historyFetchBypass = true
    )
    plugins += new DecoderPredictionPlugin(
      //      applyAt = _.pipeline.decoded,
      flushOnBranch = false //TODO remove me (DEBUG)
    )
    plugins += new BtbPlugin(
      //      entries = 8192*8,
      entries = 512,
      readAt = 0,
      hitAt = 1,
      jumpAt = 1
    )
    plugins += new GSharePlugin(
      //      entries = 1 << 24,
      memBytes = 4 KiB,
      historyWidth = 24,
      readAt = 0
    )

    //LOAD / STORE
    if(withLoadStore){
      withLsu2 match {
        case false => plugins += new LsuPlugin(
          lqSize = lqSize,
          sqSize = sqSize,
          loadToCacheBypass = true,
          lqToCachePipelined = true,
          hitPedictionEntries = 1024,
          loadWriteRfOnPrivilegeFail = sideChannels,
          translationStorageParameter = MmuStorageParameter(
            levels = List(
              MmuStorageLevel(
                id = 0,
                ways = 4,
                depth = 32
              ),
              MmuStorageLevel(
                id = 1,
                ways = 2,
                depth = 32
              )
            ),
            priority = 1
          ),

          loadTranslationParameter = withMmu match {
            case false => StaticAddressTranslationParameter(rspAt = 1)
            case true => MmuPortParameter(
              readAt = 0,
              hitsAt = 0,
              ctrlAt = 1,
              rspAt = 1
            )
          },
          storeTranslationParameter = withMmu match {
            case false => StaticAddressTranslationParameter(rspAt = 1)
            case true => MmuPortParameter(
              readAt = 1,
              hitsAt = 1,
              ctrlAt = 1,
              rspAt = 1
            )
          }
        )
        case true => plugins += new Lsu2Plugin(
          lqSize = lqSize,
          sqSize = sqSize,
          //          loadToCacheBypass = true,
          lqToCachePipelined = true,
          //          hitPedictionEntries = 1024,
          loadWriteRfOnPrivilegeFail = sideChannels,
          translationStorageParameter = MmuStorageParameter(
            levels = List(
              MmuStorageLevel(
                id = 0,
                ways = 4,
                depth = 32
              ),
              MmuStorageLevel(
                id = 1,
                ways = 2,
                depth = 32
              )
            ),
            priority = 1
          ),

          sharedTranslationParameter = withMmu match {
            case false => StaticAddressTranslationParameter(rspAt = 1)
            case true => MmuPortParameter(
              readAt = 0,
              hitsAt = 0,
              ctrlAt = 1,
              rspAt  = 1
            )
          }
        )
      }
    }

    if(!withLoadStore){
      plugins += new Plugin{
        val setup = create early new Area{
          val cache = getService[DataCachePlugin]
          val store = cache.newStorePort()
          spinal.lib.slave(store)
          spinal.lib.slave(cache.setup.lockPort)
        }
      }
    }

    plugins += new DataCachePlugin(
      memDataWidth = 64,
      cacheSize    = 4096*4,
      wayCount     = 4,
      refillCount = 4,
      writebackCount = 4,
      tagsReadAsync = withDistributedRam,
      loadReadTagsAt = if(withDistributedRam) 1 else 0,
      storeReadTagsAt = if(withDistributedRam) 1 else 0,
      reducedBankWidth = false,
      //      loadHitAt      = 2
      //      loadRspAt      = 3,
      loadRefillCheckEarly = false
    )

    //MISC
    plugins += new RobPlugin(
      robSize = robSize,
      completionWithReg = !withDistributedRam
    )
    plugins += new CommitPlugin(
      commitCount = decodeCount,
      ptrCommitRetimed = true
    )
    plugins += new RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 64,
      bankCount = 1,
      preferedWritePortForInit = "ALU0"
    )
    plugins += new CommitDebugFilterPlugin(List(4, 8, 12))
    plugins += new CsrRamPlugin()
    plugins += new PrivilegedPlugin(PrivilegedConfig.full.copy(
      withRdTime = withRdTime,
      withSupervisor = withSupervisor,
      withDebug = withDebug,
      debugTriggers = debugTriggers
    ))
    if(withPerfCounters) plugins += new PerformanceCounterPlugin(
      additionalCounterCount = 4,
      bufferWidth            = 6
    )

    //EXECUTION UNITES
    plugins += new ExecutionUnitBase("ALU0")
    plugins += new IntFormatPlugin("ALU0")
    plugins += new SrcPlugin("ALU0")
    plugins += new IntAluPlugin("ALU0", aluStage = 0)
    plugins += new ShiftPlugin("ALU0" , aluStage = 0)
    if(aluCount > 1) plugins += new BranchPlugin("ALU0")


    plugins += new ExecutionUnitBase("EU0", writebackCountMax = 1, readPhysRsFromQueue = true)
    plugins += new IntFormatPlugin("EU0")
    plugins += new SrcPlugin("EU0")
    plugins += new MulPlugin("EU0", writebackAt = 2, staticLatency = false)
    plugins += new DivPlugin("EU0", writebackAt = 2)
    //    plugins += new IntAluPlugin("EU0")
    //    plugins += new ShiftPlugin("EU0")
    if(aluCount == 1) plugins += new BranchPlugin("EU0", writebackAt = 2, staticLatency = false)
    if(withLoadStore) {
      withLsu2 match {
        case false => {
          withDedicatedLoadAgu match{
            case false => plugins += new LoadPlugin("EU0")
            case true => {
              plugins += new ExecutionUnitBase("LOAD", writebackCountMax = 0, readPhysRsFromQueue = true)
              plugins += new SrcPlugin("LOAD")
              plugins += new LoadPlugin("LOAD")
            }
          }
          plugins += new StorePlugin("EU0")
        }
        case true => plugins += new AguPlugin("EU0")
      }
    }
    plugins += new EnvCallPlugin("EU0")(rescheduleAt = 2)
    plugins += new CsrAccessPlugin("EU0")(
      writebackAt = 2
    )

    if(fpu){
      plugins += new FpuSettingPlugin(withFloat, withDouble)
      plugins += new ExecutionUnitBase("FPU0", writebackCountMax = 0, readPhysRsFromQueue = true)
      plugins += new FpuFloatExecute("FPU0")
      plugins += new RegFilePlugin(
        spec = riscv.FloatRegFile,
        physicalDepth = 64,
        bankCount = 1,
        allOne = simulation,
        preferedWritePortForInit = "Fpu"
      )

      plugins += new FpuIntegerExecute("EU0")

      plugins += new RfAllocationPlugin(riscv.FloatRegFile)
      plugins += new RfTranslationPlugin(riscv.FloatRegFile)

      plugins += new FpuWriteback()
      plugins += new FpuEmbedded()
    }

    //    plugins += new ExecutionUnitBase("EU2", writebackCountMax = 0)
    //    plugins += new SrcPlugin("EU2")
    //    plugins += new LoadPlugin("EU2")
    //
    //
    //    plugins += new ExecutionUnitBase("EU3", writebackCountMax = 0)
    //    plugins += new SrcPlugin("EU3")
    //    plugins += new StorePlugin("EU3")

    //    plugins += new ExecutionUnitBase("EU2")
    //    plugins += new MulPlugin("EU2", staticLatency = false)
    //    plugins += new DivPlugin("EU2", staticLatency = false)
    //    plugins += new SrcPlugin("EU2")
    //    plugins += new IntAluPlugin("EU2")
    //    plugins += new ShiftPlugin("EU2")
    //    plugins += new BranchPlugin("EU2")
    //    plugins += new LoadPlugin("EU2")
    //    plugins += new StorePlugin("EU2")

    if(aluCount >= 2) {
      plugins += new ExecutionUnitBase("ALU1")
      plugins += new IntFormatPlugin("ALU1")
      plugins += new SrcPlugin("ALU1")
      plugins += new IntAluPlugin("ALU1")
      plugins += new ShiftPlugin("ALU1")
      plugins += new BranchPlugin("ALU1")
      assert(aluCount < 3)
    }


    //
    //    plugins += new ExecutionUnitBase("EU5", writebackCountMax = 0)
    //    plugins += new SrcPlugin("EU5")
    //    plugins += new StorePlugin("EU5")

    //    plugins += new ExecutionUnitBase("EU5", writebackCountMax = 1)
    //    plugins += new MulPlugin("EU5", writebackAt = 2, staticLatency = false)
    //    plugins += new DivPlugin("EU5", writebackAt = 2)
    //    plugins += new EnvCallPlugin("EU5")(rescheduleAt = 2)
    //    plugins += new CsrAccessPlugin("EU5")(
    //      decodeAt = 0,
    //      readAt = 1,
    //      writeAt = 2,
    //      writebackAt = 2,
    //      staticLatency = false
    //    )


    // Integer write port sharing
    val intRfWrite = new{}
    plugins.collect{
      case lsu : LsuPlugin =>
        lsu.addRfWriteSharing(IntRegFile, intRfWrite, withReady = false, priority = 2)
      case eu0 : ExecutionUnitBase if eu0.euId == "EU0" =>
        eu0.addRfWriteSharing(IntRegFile, intRfWrite, withReady = true, priority = 1)
      case fpu : FpuWriteback =>
        fpu.addRfWriteSharing(IntRegFile, intRfWrite, withReady = true, priority = 0)
    }

    plugins
  }
}
object TGC5M_LMB extends App{
  LutInputs.set(6)
  def plugins = {
    val l = Config.plugins(
      withRdTime = false,
      aluCount    = 2,
      decodeCount = 2,
      debugTriggers = 4,
      withDedicatedLoadAgu = false,
      withRvc = false,
      withLoadStore = true,
      withMmu = true,
      withDebug = false,
      withEmbeddedJtagTap = false,
      jtagTunneled = false,
      withFloat = false,
      withDouble = false,
      withLsu2 = true,
      lqSize = 16,
      sqSize = 16,
      ioRange = a => a(31 downto 28) === 0x1// || !a(12)//(a(5, 6 bits) ^ a(12, 6 bits)) === 51
    )
    l.foreach{
      case p : EmbeddedJtagPlugin => p.debugCd.load(ClockDomain.current.copy(reset = Bool().setName("debug_reset")))
      case _ =>
    }
    l
  }

  {
    val spinalConfig = SpinalConfig(inlineRom = true, targetDirectory = "../../TGCP-RUN/verilog")
    spinalConfig.addTransformationPhase(new MemReadDuringWriteHazardPhase)
    spinalConfig.addTransformationPhase(new MemReadDuringWritePatcherPhase)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    //  spinalConfig.addTransformationPhase(new MultiPortReadSymplifier)

    spinalConfig.includeSimulation

    val report = spinalConfig.generateVerilog(new NaxRiscv(plugins))
    val doc = report.toplevel.framework.getService[DocPlugin]
    doc.genC()

    val nax = report.toplevel
    //    val dcache = nax.framework.getService[DataCachePlugin].logic.cache
  }

  {
    def wrapper[T <: Component](c: T) = {
      c.afterElaboration(c.getAllIo.foreach(_.addTag(crossClockDomain)))
      c.afterElaboration(Rtl.xorOutputs(Rtl.ffIo(c))); c
    }
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
    spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

    spinalConfig.generateVerilog(wrapper(new NaxRiscv(plugins).setDefinitionName("TGC5M_LMB_Synt")))
  }
}

