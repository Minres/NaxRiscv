package naxriscv.backend

import naxriscv.interfaces.RfAllocationService
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._

class AllocatorMultiPort[T <: Data](dataType : HardType[T], depth : Int, pushCount : Int, popCount : Int) extends Component{
  val io = new Bundle{
    val push = Vec.fill(pushCount)(slave(Flow(dataType())))
    val pop = Vec.fill(pushCount)(master(Stream(dataType())))
  }
}

class RfAllocationPlugin extends Plugin with RfAllocationService{

  override def newAllocPort() = ???
  override def newFreePort() = ???

  val logic = create late new Area{
    val entryCount : Int = ???
    val entryIdWidth = log2Up(entryCount)
    val entryType = HardType(UInt(entryIdWidth bits))
    val allocator = new AllocatorMultiPort(entryType, entryCount, ???, ???)
  }
}
