package naxriscv.tgfs

import naxriscv._
import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.fetch._
import naxriscv.lsu._
import naxriscv.misc._
import naxriscv.utilities._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite.AxiLite4Utils.toAxiConfig
import spinal.lib.bus.amba4.axilite._

object Axi4LReadOnlyArbiter{
  def getInputConfig(outputConfig : AxiLite4Config, inputsCount : Int) = outputConfig.copy()
}

case class Axi4LReadOnlyArbiter(outputConfig: AxiLite4Config,inputsCount : Int) extends Component {
  val inputConfig = Axi4LReadOnlyArbiter.getInputConfig(outputConfig,inputsCount)
  val io = new Bundle{
    val inputs = Vec(slave(AxiLite4ReadOnly(inputConfig)),inputsCount)
    val output = master(AxiLite4ReadOnly(outputConfig))
  }

  val target = AxiLite4ReadOnly(outputConfig)
  val cmdArbiter = StreamArbiterFactory().roundRobin.build(AxiLite4Ax(inputConfig),inputsCount)
  (cmdArbiter.io.inputs,io.inputs.map(_.readCmd)).zipped.map(_ <> _)
  cmdArbiter.io.output <> target.readCmd
  val (t1, t2) = StreamFork2(target.readCmd, synchronous=false)
  io.output.readCmd <> t1

  val idIn = Stream(UInt (log2Up(inputsCount) bit))
  val idSkid = idIn.s2mPipe(8)

  idIn.valid := t2.valid
  idIn.payload := cmdArbiter.io.chosen
  t2.ready := idIn.ready

  // Route readResp
  val readRspIndex = idSkid.payload
  val readRspSels = (0 until inputsCount).map(readRspIndex === _)
  for((input,sel)<- (io.inputs,readRspSels).zipped){
    input.readRsp.valid := io.output.readRsp.valid && idSkid.valid && sel
    input.readRsp.payload <> io.output.readRsp.payload
  }
  io.output.readRsp.ready := io.inputs(readRspIndex).readRsp.ready
  idSkid.ready := io.inputs(readRspIndex).readRsp.ready && idSkid.valid
}

object TgcUtils {

  def assignDefaultDriver(bundle: Bundle): Unit = {
    for ((name, element) <- bundle.elements) {
        element match {
          case b: Bundle => assignDefaultDriver(b)
          case bv: Bits  => if(!bv.hasAssignement) bv := B(0).resized
          case si: SInt  => if(!si.hasAssignement) si := S(0).resized
          case ui: UInt  => if(!ui.hasAssignement) ui := U(0).resized
          case bl: Bool  => if(!bl.hasAssignement) bl := False
        }
    }
  }

  implicit class AxiLite4Conv(axiLite: AxiLite4) {
    def toAxi4(cfg: Axi4Config): Axi4 = {
      val axi = Axi4(cfg.copy(addressWidth = axiLite.config.addressWidth, dataWidth = axiLite.config.dataWidth))

      axi.aw.arbitrationFrom(axiLite.aw)
      axi.aw.payload.assignSomeByName(axiLite.aw.payload)
      axi.aw.size := U(log2Up(axiLite.config.dataWidth/8))
      axi.w.arbitrationFrom(axiLite.w)
      axi.w.payload.assignSomeByName(axiLite.w.payload)
      axi.w.last := True
      axiLite.b.arbitrationFrom(axi.b)
      axiLite.b.payload.assignSomeByName(axi.b.payload)

      axi.ar.arbitrationFrom(axiLite.ar)
      axi.ar.payload.assignSomeByName(axiLite.ar.payload)
      axi.ar.size := U(log2Up(axiLite.config.dataWidth/8))
      axiLite.r.arbitrationFrom(axi.r)
      axiLite.r.payload.assignSomeByName(axi.r.payload)

      assignDefaultDriver(axi.aw.payload)
      assignDefaultDriver(axi.w.payload)
      assignDefaultDriver(axi.ar.payload)

      axi
    }
  }
  implicit class AxiLite4ReadOnlyConv(axiLite: AxiLite4ReadOnly) {
    def toAxi4(cfg: Axi4Config): Axi4ReadOnly = {
      val axi = Axi4ReadOnly(cfg.copy(addressWidth = axiLite.config.addressWidth, dataWidth = axiLite.config.dataWidth))

      axi.ar.arbitrationFrom(axiLite.ar)
      axi.ar.payload.assignSomeByName(axiLite.ar.payload)
      axi.ar.size := U(log2Up(axiLite.config.dataWidth/8))

      axiLite.r.arbitrationFrom(axi.r)
      axiLite.r.payload.assignSomeByName(axi.r.payload)

      assignDefaultDriver(axi.ar.payload)

      axi
    }
  }

}
