package naxriscv.tgfs

import naxriscv._
import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.fetch._
import naxriscv.lsu._
import naxriscv.misc._
import naxriscv.utilities._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Config, _}
import spinal.lib.bus.amba4.axilite.AxiLite4Utils._
import naxriscv.tgfs.TgcUtils._

import scala.collection.mutable.ArrayBuffer

object NaxRiscvTgcB {
  def apply(plugins: ArrayBuffer[Plugin], xlen: Int, toPeripheral: UInt => Bool): NaxRiscvTgcB =
    new NaxRiscvTgcB(plugins, xlen, toPeripheral)
}

class NaxRiscvTgcB(plugins: ArrayBuffer[Plugin], xlen: Int, toPeripheral: UInt => Bool) extends Component {
  val io = new Bundle {
    var axi4_ibus: Axi4ReadOnly = _
    var axi4_dbus :Axi4 = _

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
  plugins += new LsuPeripheralAxiLite4(ioDataWidth, reg_stage_cmd = false, reg_stage_ret = false)
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

  val ibus = new Area {
    val ch_ibus = core.framework.getService[FetchAxi4].logic.axiRam
    val nc_ibus = core.framework.getService[FetchAxi4].logic.axiPeripheral.toAxi4(ch_ibus.config)
    val resizer = Axi4ReadOnlyUpsizer(nc_ibus.config, ch_ibus.config, pendingQueueSize = 4)
    resizer.io.input << nc_ibus
    val axi4_arbiter = Axi4ReadOnlyArbiter(ch_ibus.config.copy(idWidth = (ch_ibus.config.idWidth max nc_ibus.config.idWidth) + 1), inputsCount = 2)
    axi4_arbiter.io.inputs(0) << ch_ibus
    axi4_arbiter.io.inputs(1) << resizer.io.output

    io.axi4_ibus = master(Axi4ReadOnly(axi4_arbiter.outputConfig)).setName("iBus")
    io.axi4_ibus << axi4_arbiter.io.output
    Axi4SpecRenamer(io.axi4_ibus)
  }

  val dbus = new Area {
    val ch_dbus = core.framework.getService[DataCacheAxi4].logic.axi
    val nc_dbus = core.framework.getService[LsuPeripheralAxiLite4].logic.axi.toAxi4(ch_dbus.config)

    val rresizer = Axi4ReadOnlyUpsizer(nc_dbus.config, ch_dbus.config, pendingQueueSize = 4)
    rresizer.io.input.readCmd << nc_dbus.readCmd
    rresizer.io.input.readRsp >> nc_dbus.readRsp
    val rarbiter = Axi4ReadOnlyArbiter(ch_dbus.config.copy(idWidth = (ch_dbus.config.idWidth max nc_dbus.config.idWidth) + 1), inputsCount = 2)
    rarbiter.io.inputs(0).readCmd << ch_dbus.readCmd
    rarbiter.io.inputs(0).readRsp >> ch_dbus.readRsp
    rarbiter.io.inputs(1) << rresizer.io.output

    val wresizer = Axi4WriteOnlyUpsizer(nc_dbus.config, ch_dbus.config)
    wresizer.io.input.writeCmd << nc_dbus.writeCmd
    wresizer.io.input.writeData << nc_dbus.writeData
    wresizer.io.input.writeRsp >> nc_dbus.writeRsp
    val warbiter = Axi4WriteOnlyArbiter(ch_dbus.config.copy(idWidth = (ch_dbus.config.idWidth max nc_dbus.config.idWidth) + 1), inputsCount = 2, routeBufferSize = 4)
    warbiter.io.inputs(0).writeCmd << ch_dbus.writeCmd
    warbiter.io.inputs(0).writeData<< ch_dbus.writeData
    warbiter.io.inputs(0).writeRsp >> ch_dbus.writeRsp
    warbiter.io.inputs(1) << wresizer.io.output

    io.axi4_dbus = master(Axi4(warbiter.outputConfig)).setName("dBus")
    io.axi4_dbus.readCmd << rarbiter.io.output.readCmd
    io.axi4_dbus.readRsp >> rarbiter.io.output.readRsp
    io.axi4_dbus.writeCmd << warbiter.io.output.writeCmd
    io.axi4_dbus.writeData << warbiter.io.output.writeData
    io.axi4_dbus.writeRsp >> warbiter.io.output.writeRsp
    Axi4SpecRenamer(io.axi4_dbus)
  }
  val priv = core.framework.getService[PrivilegedPlugin].io
  priv.int.machine.timer := io.timIrq
  priv.int.machine.software := io.swIrq
  priv.int.machine.external := io.extIrq
  priv.int.supervisor.external := io.extIrqSv
  priv.rdtime := io.rdtime
}

