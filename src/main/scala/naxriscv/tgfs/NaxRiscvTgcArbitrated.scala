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
import spinal.lib.bus.amba4.axilite._

import scala.collection.mutable.ArrayBuffer

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

object NaxRiscvTgcArbitrated {
  def apply(plugins: ArrayBuffer[Plugin], xlen: Int, toPeripheral: UInt => Bool): NaxRiscvTgcArbitrated =
    new NaxRiscvTgcArbitrated(plugins, xlen, toPeripheral)
}

class NaxRiscvTgcArbitrated(plugins: ArrayBuffer[Plugin], xlen: Int, toPeripheral: UInt => Bool) extends Component {
  val io = new Bundle {
//    var axi4_bus: Axi4 = _
//    var axi4l_bus :AxiLite4 = _

    val timIrq = in Bool()
    val swIrq = in Bool()
    val extIrq = in Bool()
    val extIrqSv = in Bool()
    val rdtime = in UInt(64 bits)
  }
  noIoPrefix()

  val ramDataWidth = 64
  val ioDataWidth = xlen
  plugins += new FetchAxi4(
    ramDataWidth = ramDataWidth,
    ioDataWidth = ioDataWidth,
    toPeripheral = cmd => toPeripheral(cmd.address)
  )
  plugins += new DataCacheAxi4(
    dataWidth = ramDataWidth
  )
  plugins += new LsuPeripheralAxiLite4(ioDataWidth)
  val core = new NaxRiscv(plugins)
  val fetch = core.framework.getService[FetchAxi4]

  plugins.foreach {
    case p: EmbeddedJtagPlugin => {
      if (p.withTap) p.logic.jtag.toIo().setName("jtag")
      else p.logic.jtagInstruction.toIo().setName("jtag_instruction")
      p.logic.ndmreset.toIo().setName("debug_ndmreset")
    }
    case _ =>
  }

  val coherent = new Area {
    val ch_ibus = core.framework.getService[FetchAxi4].logic.axiRam
    val ch_dbus = core.framework.getService[DataCacheAxi4].logic.axi
    val axi4_arbiter = Axi4ReadOnlyArbiter(ch_dbus.config.copy(idWidth = (ch_dbus.config.idWidth max ch_ibus.config.idWidth) + 1), inputsCount = 2)
    axi4_arbiter.io.inputs(0) << ch_ibus
    axi4_arbiter.io.inputs(1) << ch_dbus.toReadOnly()

    val axi4_bus = master(Axi4(axi4_arbiter.outputConfig)).setName("mem")
    axi4_bus << axi4_arbiter.io.output
    axi4_bus << ch_dbus.toWriteOnly()
    Axi4SpecRenamer(axi4_bus)
  }
  val noncoherent = new Area {
    val nc_ibus = core.framework.getService[FetchAxi4].logic.axiPeripheral
    val nc_dbus = core.framework.getService[LsuPeripheralAxiLite4].logic.axi
    AxiLite4SpecRenamer(nc_ibus)
    AxiLite4SpecRenamer(nc_dbus)

    val arbiter = Axi4LReadOnlyArbiter(nc_dbus.config, inputsCount = 2)
    arbiter.io.inputs(0).readCmd << nc_ibus.readCmd
    arbiter.io.inputs(0).readRsp >> nc_ibus.readRsp
    arbiter.io.inputs(1).readCmd << nc_dbus.readCmd
    arbiter.io.inputs(1).readRsp >> nc_dbus.readRsp

    val axi4l_bus = master(AxiLite4(arbiter.outputConfig)).setName("per")
    axi4l_bus.readCmd << arbiter.io.output.readCmd
    axi4l_bus.readRsp >> arbiter.io.output.readRsp
    axi4l_bus.writeCmd << nc_dbus.writeCmd
    axi4l_bus.writeData << nc_dbus.writeData
    axi4l_bus.writeRsp >> nc_dbus.writeRsp
    AxiLite4SpecRenamer(axi4l_bus)
  }
  val priv = core.framework.getService[PrivilegedPlugin].io
  priv.int.machine.timer := io.timIrq
  priv.int.machine.software := io.swIrq
  priv.int.machine.external := io.extIrq
  priv.int.supervisor.external := io.extIrqSv
  priv.rdtime := io.rdtime
}
