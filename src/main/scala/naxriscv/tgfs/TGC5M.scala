package naxriscv.tgfs

import naxriscv._
import naxriscv.fetch._
import naxriscv.lsu._
import naxriscv.misc._
import naxriscv.utilities._
import naxriscv.compatibility.{EnforceSyncRamPhase, MemReadDuringWriteHazardPhase, MemReadDuringWritePatcherPhase, MultiPortWritesSymplifier}
import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.platform.ScalaInterpreter.evaluate
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4ReadOnlyArbiter, Axi4SpecRenamer}
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.{AxiLite4Clint, WishboneClint}
import spinal.lib.misc.plic.{AxiLite4Plic, WishbonePlic}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class NaxRiscvLitex(plugins : ArrayBuffer[Plugin], xlen : Int, toPeripheral : UInt => Bool) extends Component{

  val ramDataWidth = 64
  val ioDataWidth  = 32
  plugins += new FetchAxi4(
    ramDataWidth = ramDataWidth,
    ioDataWidth  = ioDataWidth,
    toPeripheral = cmd => toPeripheral(cmd.address)
  )
  plugins += new DataCacheAxi4(
    dataWidth = ramDataWidth
  )
  plugins += new LsuPeripheralAxiLite4(
    ioDataWidth  = ioDataWidth
  )

  val core = new NaxRiscv(
    plugins
  )

  val priv = core.framework.getService[PrivilegedPlugin].io
  val timIrq = in Bool()
  val swIrq = in Bool()
  val extIrq = in Bool()
  val extIrqSv = if(priv.int.supervisor != null) in Bool() else null
  val rdtime = in Bits(64 bits)

  val ch = new Area{
    val ibus = core.framework.getService[FetchAxi4].logic.axiRam.toIo()
    val dbus = core.framework.getService[DataCacheAxi4].logic.axi.toIo()
    Axi4SpecRenamer(ibus)
    Axi4SpecRenamer(dbus)

    plugins.foreach{
      case p : EmbeddedJtagPlugin => {
        if(p.withTap) p.logic.jtag.toIo().setName("jtag")
        else p.logic.jtagInstruction.toIo().setName("jtag_instruction")
        p.logic.ndmreset.toIo().setName("debug_ndmreset")
      }
      case _ =>
    }

//    val ibus = cpu.framework.getService[FetchAxi4].logic.axiRam
//    val dbus = cpu.framework.getService[DataCacheAxi4].logic.axi
//
//    val arbiter = Axi4ReadOnlyArbiter(dbus.config.copy(idWidth = (dbus.config.idWidth max ibus.config.idWidth) + 1), inputsCount = 2)
//    arbiter.io.inputs(0) << ibus
//    arbiter.io.inputs(1) << dbus.toReadOnly()
//
//    val bus = master(Axi4(arbiter.outputConfig))
//    bus << arbiter.io.output
//    bus << dbus.toWriteOnly()
//
//    Axi4SpecRenamer(bus)
  }

  val nc = new Area{
    val ibus = core.framework.getService[FetchAxi4].logic.axiPeripheral.toIo()
    val dbus = core.framework.getService[LsuPeripheralAxiLite4].logic.axi.toIo()
    AxiLite4SpecRenamer(ibus)
    AxiLite4SpecRenamer(dbus)


    priv.int.machine.timer       := timIrq
    priv.int.machine.software    := swIrq
    priv.int.machine.external    := extIrq
    if(priv.int.supervisor != null) priv.int.supervisor.external := extIrqSv
    priv.rdtime                  := rdtime.asUInt
  }
}

case class LitexMemoryRegion(mapping : SizeMapping, mode : String, bus : String){
  def isIo = mode.contains("i") || mode.contains("o")
  def isExecutable = mode.contains("x")
  def isCachable = mode.contains("c")
  def onPeripheral = bus match {
    case "m" => false
    case "p" => true
  }
  def onMemory = !onPeripheral
}

object TGC5M extends App{
  var netlistDirectory = "../../TGCP-RUN/verilog"
  var netlistName = "TGC5M"
  var resetVector = 0l
  var xlen = 32
  var jtagTap = false
  var jtagInstruction = false
  var debug = false
  val files = ArrayBuffer[String]()
  val scalaArgs = ArrayBuffer[String]()
  val memoryRegions = ArrayBuffer[LitexMemoryRegion]()

  val spinalConfig = SpinalConfig(inlineRom = true, targetDirectory = netlistDirectory)
  spinalConfig.addTransformationPhase(new MemReadDuringWriteHazardPhase)
  spinalConfig.addTransformationPhase(new MemReadDuringWritePatcherPhase)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
  // spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
  // spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)
  spinalConfig.includeSimulation
  val report = spinalConfig.generateSystemVerilog {
    LutInputs.set(6)
    def plugins = {
      val l = Config.plugins(
        withRdTime = true,
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
        ioRange =    a => a(31 downto 28) === 0xf,
        fetchRange = a => a(31 downto 28) =/= 0xf

      )
      l.foreach{
        case p : EmbeddedJtagPlugin => p.debugCd.load(ClockDomain.current.copy(reset = Bool().setName("debug_reset")))
        case _ =>
      }
      l
    }
    new NaxRiscvLitex(plugins, xlen, address => memoryRegions.filter(_.onPeripheral).map(_.mapping.hit(address)).orR).setDefinitionName(netlistName)
  }
  val doc = report.toplevel.core.framework.getService[DocPlugin]
  doc.genC(netlistDirectory + "/" + netlistName + ".h")
}