package naxriscv.tgcp

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

object NaxRiscvAxi {
  def apply(plugins: ArrayBuffer[Plugin], xlen: Int, toPeripheral: UInt => Bool): NaxRiscvAxi =
    new NaxRiscvAxi(plugins, xlen, toPeripheral)
}

class NaxRiscvAxi(plugins: ArrayBuffer[Plugin], xlen: Int, toPeripheral: UInt => Bool) extends Component {
  val io = new Bundle {
    var mem_ibus: Axi4ReadOnly = _
    var mem_dbus: Axi4 = _
    var io_ibus :AxiLite4ReadOnly = _
    var io_dbus :AxiLite4 = _

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
  io.mem_ibus = core.framework.getService[FetchAxi4].logic.axiRam.toIo().setName("ch_ibus")
  io.mem_dbus = core.framework.getService[DataCacheAxi4].logic.axi.toIo().setName("ch_dbus")
  Axi4SpecRenamer(io.mem_ibus)
  Axi4SpecRenamer(io.mem_dbus)

  plugins.foreach {
    case p: EmbeddedJtagPlugin => {
      if (p.withTap) p.logic.jtag.toIo().setName("jtag")
      else p.logic.jtagInstruction.toIo().setName("jtag_instruction")
      p.logic.ndmreset.toIo().setName("debug_ndmreset")
    }
    case _ =>
  }

//    val ibus = cpu.framework.getService[FetchAxi4].logic.axiRam.toIo()
//    val dbus = cpu.framework.getService[DataCacheAxi4].logic.axi.toIo()
//
//    val arbiter = Axi4ReadOnlyArbiter(dbus.config.copy(idWidth = (dbus.config.idWidth max ibus.config.idWidth) + 1), inputsCount = 2)
//    arbiter.io.inputs(0) << ibus
//    arbiter.io.inputs(1) << dbus.toReadOnly()
//
//    val bus = master(Axi4(arbiter.outputConfig)).setName("mem")
//    bus << arbiter.io.output
//    bus << dbus.toWriteOnly()
//
//    Axi4SpecRenamer(bus)

  io.io_ibus = core.framework.getService[FetchAxi4].logic.axiPeripheral.toIo().setName("nc_ibus")
  io.io_dbus = core.framework.getService[LsuPeripheralAxiLite4].logic.axi.toIo().setName("nc_dbus")
  AxiLite4SpecRenamer(io.io_ibus)
  AxiLite4SpecRenamer(io.io_dbus)
//    val ibus = cpu.framework.getService[FetchAxi4].logic.axiPeripheral
//    val dbus = cpu.framework.getService[LsuPeripheralAxiLite4].logic.axi
//    val arbiter = Axi4LReadOnlyArbiter(dbus.config, inputsCount = 2)
//    arbiter.io.inputs(0).readCmd << ibus.readCmd
//    arbiter.io.inputs(0).readRsp >> ibus.readRsp
//    arbiter.io.inputs(1).readCmd << dbus.readCmd
//    arbiter.io.inputs(1).readRsp >> dbus.readRsp
//
//    val bus = master(AxiLite4(arbiter.outputConfig)).setName("io")
//    bus.readCmd << arbiter.io.output.readCmd
//    bus.readRsp >> arbiter.io.output.readRsp
//    bus.writeCmd << dbus.writeCmd
//    bus.writeData << dbus.writeData
//    bus.writeRsp >> dbus.writeRsp

  val priv = core.framework.getService[PrivilegedPlugin].io
  priv.int.machine.timer := io.timIrq
  priv.int.machine.software := io.swIrq
  priv.int.machine.external := io.extIrq
  priv.int.supervisor.external := io.extIrqSv
  priv.rdtime := io.rdtime
}
