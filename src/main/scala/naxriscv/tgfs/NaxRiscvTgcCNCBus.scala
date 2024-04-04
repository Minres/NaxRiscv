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

class NaxRiscvTgcCNCBus(plugins: ArrayBuffer[Plugin], xlen: Int, toPeripheral: UInt => Bool) extends Component {
  val io = new Bundle {
    var axi4_io: AxiLite4 = _
    var axi4_mem :Axi4 = _
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

    io.axi4_mem = master(Axi4(axi4_arbiter.outputConfig)).setName("mem")
    io.axi4_mem << axi4_arbiter.io.output
    io.axi4_mem << ch_dbus.toWriteOnly()
    Axi4SpecRenamer(io.axi4_mem)
  }
  val noncoherent = new Area {
    val nc_ibus = core.framework.getService[FetchAxi4].logic.axiPeripheral
    val nc_dbus = core.framework.getService[LsuPeripheralAxiLite4].logic.axi
    val arbiter = Axi4LReadOnlyArbiter(nc_dbus.config, inputsCount = 2)
    arbiter.io.inputs(0).readCmd << nc_ibus.readCmd
    arbiter.io.inputs(0).readRsp >> nc_ibus.readRsp
    arbiter.io.inputs(1).readCmd << nc_dbus.readCmd
    arbiter.io.inputs(1).readRsp >> nc_dbus.readRsp

    io.axi4_io = master(AxiLite4(arbiter.outputConfig)).setName("per")
    io.axi4_io.readCmd << arbiter.io.output.readCmd
    io.axi4_io.readRsp >> arbiter.io.output.readRsp
    io.axi4_io.writeCmd << nc_dbus.writeCmd
    io.axi4_io.writeData << nc_dbus.writeData
    io.axi4_io.writeRsp >> nc_dbus.writeRsp
    AxiLite4SpecRenamer(io.axi4_io)
  }
  val priv = core.framework.getService[PrivilegedPlugin]
  val priv_io = priv.io
  priv_io.int.machine.timer := io.timIrq
  priv_io.int.machine.software := io.swIrq
  priv_io.int.machine.external := io.extIrq
  if(priv.implementSupervisor)
    priv_io.int.supervisor.external := io.extIrqSv
  priv_io.rdtime := io.rdtime
}
