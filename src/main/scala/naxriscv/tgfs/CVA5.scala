package naxriscv.tgfs

import naxriscv.lsu.{DataCachePlugin, DataLoadPort, DataStorePort}
import naxriscv.utilities.{DataBase, Framework, NaxScope, Plugin}
import spinal.core._
import spinal.lib.experimental.chisel.Bundle

class MemorySubUnitInterface(addrWidth: Int, dataWidth: Int) extends Bundle {
  val new_request = out Bool()
  val addr = out UInt(addrWidth bits)
  val re = out Bool()
  val we = out Bool()
  val be = out Bits(dataWidth/8 bits)
  val data_in = out Bits(dataWidth bits)

  val data_out = in Bits(dataWidth bits)
  val data_valid = in Bool()
  val ready = in Bool()

  def connect(load: DataLoadPort, store: DataStorePort): Unit = {
    store.cmd.valid := new_request && we
    store.cmd.payload.address := addr
    store.cmd.payload.data := data_in
    store.cmd.payload.mask := be

    //ToDo:
    store.cmd.payload.generation := False
    store.cmd.payload.io := False
    store.cmd.payload.flush := False
    store.cmd.payload.flushFree := False
    store.cmd.payload.prefetch := False

    load.cmd.valid := new_request && re
    load.cmd.payload.virtual := addr
    load.translated.physical := addr

    //ToDo:
    load.translated.abord := False
    load.cancels := 0

    //ToDo:
    load.cmd.payload.size := 1
    load.cmd.payload.redoOnDataHazard := False
    load.cmd.payload.unlocked := False
    load.cmd.payload.unique := False

    ready := store.cmd.ready && load.cmd.ready
    data_valid := load.rsp.valid
    data_out := load.rsp.payload.data
  }
}

class CVA5Wrapper(val plugins : Seq[Plugin]) extends Component {
  val database = new DataBase
  val framework = NaxScope(database) on new Framework(plugins)
  setName("core")
}

object CVA5 {
  def plugins = {
    Seq[Plugin] (
      new DataCachePlugin(
        memDataWidth = 64,
        cacheSize = 4096 * 4,
        wayCount = 4,
        refillCount = 2,
        writebackCount = 2,
        tagsReadAsync = true,
        loadReadTagsAt = 1,
        storeReadTagsAt = 1,
        reducedBankWidth = false,
        loadRefillCheckEarly = false,
        withCoherency = true,
        probeIdWidth = 4,
        ackIdWidth = 4
      ),
      new MyFetchCachePlugin(
        memDataWidth = 64,
        cacheSize = 4096 * 4,
        wayCount = 4,
        refillCount = 2,
        writebackCount = 2,
        tagsReadAsync = true,
        loadReadTagsAt = 1,
        storeReadTagsAt = 1,
        reducedBankWidth = false,
        loadRefillCheckEarly = false,
        withCoherency = true,
        probeIdWidth = 4,
        ackIdWidth = 4
      ),
      new CVA5BlackBoxPlugin()
    )
  }
}

case class CVA5() extends BlackBox {

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val dgeneric = new MemorySubUnitInterface(32, 32)
    val igeneric = new MemorySubUnitInterface(32, 32)
  }

  mapCurrentClockDomain(io.clk, io.rst)
  //ClockDomainTag(this.clockDomain)(io)
  noIoPrefix()
  //ToDo: Add all required sv files
  addRTLPath("abc.sv")
}

class CVA5BlackBoxPlugin extends Plugin {
  val logic = create config new Area{
    val blackBox = new CVA5()
  }
}
