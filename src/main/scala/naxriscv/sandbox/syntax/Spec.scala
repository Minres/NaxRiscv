package naxriscv.sandbox.syntax

import naxriscv.interfaces.{RegfileSpec}
import naxriscv.units.ExecuteUnitDemo
import spinal.core._
//object Miaou{
//  class Resource
//  class Arg(mapping : Seq[Any]) extends Resource
//  class RfAccess
//  class RfRead extends RfAccess
//  class RfWrite extends RfAccess
//  class RegFileSpec
//  class RfResource(rf : RegFileSpec, access : RfAccess) extends Resource
//
//  val RS1 = new RfRead
//  val RS2 = new RfRead
//  val RD  = new RfWrite
//
//  val INT = new RegFileSpec
//  val INT_RS1 = new RfResource(INT, RS1)
//  val INT_RS2 = new RfResource(INT, RS2)
//  val INT_RD = new RfResource(INT, RD)
//  val TypeIArg = new Arg(List((31 downto 20) -> (11 downto 0)))
//
//  val RISCV_ADD = DirectTranslation(M"101011", List(INT_RS1, INT_RS2, INT_RD, TypeIArg))
//
//  class MicroOp
//  case class DirectTranslation(key : MaskedLiteral, ressources : Seq[Resource]) extends MicroOp
//  case class MultiTranslation(key : MaskedLiteral, uop : Seq[MicroOp]) extends MicroOp
//
//
//
//  trait RiscvDecoderService{
//    def addTranslation(dt : DirectTranslation)
//    def addDispatch(dt : MicroOp, eu : ExecuteUnit)
//  }
//
//}

//class Spec {
//
//  val intRegfile = new RegfileConfig {
//    override def size = 32
//    override def width = ???
//    override def x0AlwaysZero = true
//  }
//
//  val INT_RS1 = new RfRead{
//    def decode(i : Bits) = i(15 downto 10)
//    def rf = intRegfile
//  }
//  val INT_RD = new RfWrite{
//    def decode(i : Bits) = i(9 downto 5)
//    def rf = intRegfile
//  }
//
//  def TypeR(key : MaskedLiteral): Unit = Instruction(
//    key = key,
//    ressources = List(INT_RS1, INT_RS2, INT_RD),
//  )
//
//  val ADD = TypeR(M"010101010000")
//
//  class FuAluPlugin{
//    val setup = create early new Area {
//      decoder.add(
//        fu = this,
//        instruction = ADD
//      )
//    }
//  }
//
//  backend.addFu(new FuAluPlugin)
//}
//



//backend = List(
//ALU_OP -> ADD
//)