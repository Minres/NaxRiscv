package naxriscv.tgfs

import naxriscv.Global.{PHYSICAL_WIDTH, VIRTUAL_EXT_WIDTH, VIRTUAL_WIDTH, XLEN}
import naxriscv.lsu.DataCachePlugin
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core.fiber.{Fiber, Handle}
import spinal.core.{Area, BooleanPimped, False}
import spinal.lib.bus.tilelink.S2mSupport
import spinal.lib.bus.tilelink.fabric.Node

import scala.collection.mutable.ArrayBuffer

class TilelinkCVA5Fiber() extends Area {
  val iBus = Node.master()
  val dBus = Node.master()
  val buses = List(iBus,dBus)

  val plugins = Handle[Seq[Plugin]]
  def setPlugins(p : Seq[Plugin]) : this.type = {
    plugins.load(p)
    this
  }

  val thread = Fiber build new Area {
    val l = ArrayBuffer[Plugin]()

    l ++= plugins

    l += new Plugin {
      create config new Area {
        PHYSICAL_WIDTH.set(32)
        XLEN.set(32)
        VIRTUAL_WIDTH.set((PHYSICAL_WIDTH.get + 1) min XLEN.get)
        VIRTUAL_EXT_WIDTH.set(VIRTUAL_WIDTH.get +  (VIRTUAL_WIDTH < XLEN).toInt)
      }
    }

    l += new DocPlugin()

    /*l += new FetchCachePlugin(
      cacheSize = 4096*4,
      wayCount = 4,
      injectionAt = 2,
      fetchDataWidth = 32,
      memDataWidth = 64,
      reducedBankWidth = false,
      hitsWithTranslationWays = true,
      tagsReadAsync = true,
      //ToDo:
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
      translationPortParameter = StaticAddressTranslationParameter(rspAt = 1)
    )*/

    l += new Plugin {
      val setup = create early new Area{
        framework.plugins.foreach{
          //case p : FetchCachePlugin => iBus.m2s.forceParameters(p.getBusParameter().toTileLinkM2sParameters(p))
          case p : DataCachePlugin => {
            dBus.m2s.forceParameters(p.setup.dataCacheParameters.memParameter.toTileLinkM2sParameters(p))
            val store = p.newStorePort()
            val load = p.newLoadPort(0)
            val cva5 = getService[CVA5BlackBoxPlugin]
            cva5.logic.blackBox.io.dgeneric.connect(load, store)
          }
          case p : MyFetchCachePlugin => {
            iBus.m2s.forceParameters(p.setup.dataCacheParameters.memParameter.toTileLinkM2sParameters(p))
            val store = p.newStorePort()
            val load = p.newLoadPort(0)
            val cva5 = getService[CVA5BlackBoxPlugin]
            cva5.logic.blackBox.io.igeneric.connect(load, store)
          }
          case _ =>
        }

        framework.plugins.foreach{
          //case p : FetchCachePlugin => iBus.s2m.supported.load(S2mSupport.none())
          case p: DataCachePlugin => p.withCoherency match {
            case false => dBus.s2m.supported.load(S2mSupport.none())
            case true => dBus.s2m.supported.load(p.setup.dataCacheParameters.toTilelinkS2mSupported(dBus.s2m.proposed))
          }
          case p: MyFetchCachePlugin => p.withCoherency match {
            case false => iBus.s2m.supported.load(S2mSupport.none())
            case true => iBus.s2m.supported.load(p.setup.dataCacheParameters.toTilelinkS2mSupported(iBus.s2m.proposed))
          }
          case _ =>
        }

        framework.plugins.foreach{
          case p: DataCachePlugin =>
            if(p.withCoherency)p.setCoherencyInfo(dBus.m2s.parameters.sourceWidth, dBus.s2m.parameters.sinkWidth)
            p.lockPort.valid := False
            p.lockPort.address.assignDontCare()
          case p: MyFetchCachePlugin =>
            if(p.withCoherency)p.setCoherencyInfo(iBus.m2s.parameters.sourceWidth, iBus.s2m.parameters.sinkWidth)
            p.lockPort.valid := False
            p.lockPort.address.assignDontCare()
          case _ =>
        }
      }
    }

    val core = new CVA5Wrapper(l)

    core.plugins.foreach{
      //case p : FetchCachePlugin => iBus.bus << p.mem.toTilelink()
      case p : DataCachePlugin =>
        dBus.bus << p.mem.toTilelink()
      case p : MyFetchCachePlugin =>
        iBus.bus << p.mem.toTilelink()
      case _ =>
    }
  }
}
