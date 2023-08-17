package naxriscv.tgfs

import naxriscv._
import naxriscv.fetch._
import naxriscv.lsu._
import naxriscv.misc._
import naxriscv.utilities._
import naxriscv.debug.EmbeddedJtagPlugin
import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer

import scala.collection.mutable.ArrayBuffer

class NaxRiscvTgc(plugins : ArrayBuffer[Plugin], xlen : Int, toPeripheral : UInt => Bool) extends Component {
  val ramDataWidth = 64
  val ioDataWidth = 32
  plugins += new FetchAxi4(
    ramDataWidth = ramDataWidth,
    ioDataWidth = ioDataWidth,
    toPeripheral = cmd => toPeripheral(cmd.address)
  )
  plugins += new DataCacheAxi4(
    dataWidth = ramDataWidth
  )
  plugins += new LsuPeripheralAxiLite4(
    ioDataWidth = ioDataWidth
  )

  val core = new NaxRiscv(plugins)

  val priv = core.framework.getService[PrivilegedPlugin].io
  val timIrq = in Bool()
  val swIrq = in Bool()
  val extIrq = in Bool()
  val extIrqSv = if (priv.int.supervisor != null) in Bool() else null
  val rdtime = in Bits (64 bits)

  val ch = new Area {
    val ibus = core.framework.getService[FetchAxi4].logic.axiRam.toIo()
    val dbus = core.framework.getService[DataCacheAxi4].logic.axi.toIo()
    Axi4SpecRenamer(ibus)
    Axi4SpecRenamer(dbus)

    plugins.foreach {
      case p: EmbeddedJtagPlugin => {
        if (p.withTap) p.logic.jtag.toIo().setName("jtag")
        else p.logic.jtagInstruction.toIo().setName("jtag_instruction")
        p.logic.ndmreset.toIo().setName("debug_ndmreset")
      }
      case _ =>
    }

    val nc = new Area {
      val ibus = core.framework.getService[FetchAxi4].logic.axiPeripheral.toIo()
      val dbus = core.framework.getService[LsuPeripheralAxiLite4].logic.axi.toIo()
      AxiLite4SpecRenamer(ibus)
      AxiLite4SpecRenamer(dbus)


      priv.int.machine.timer := timIrq
      priv.int.machine.software := swIrq
      priv.int.machine.external := extIrq
      if (priv.int.supervisor != null) priv.int.supervisor.external := extIrqSv
      priv.rdtime := rdtime.asUInt
    }
  }
}
