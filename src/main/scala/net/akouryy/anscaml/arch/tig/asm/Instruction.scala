package net.akouryy.anscaml
package arch.tig.asm

import base._
import shapeless._

sealed trait Instruction {
  final def mapXID(fn: XID => XID): Instruction = {
    //noinspection TypeAnnotation
    object inc extends Poly1 {
      implicit def caseXID = at[XID](fn)

      implicit def caseXReg = at[XReg](fn)

      implicit def caseXVar = at[XVar](fn)
    }
    val res = everywhere(inc)(this) // shapelessの?バグのため一時変数に代入
    res
  }

  final def usedXID: Seq[XID] = this match {
    case Mv(value) =>
      Seq(value)
    case _: Mvi | Nop | Read =>
      Seq()
    case NewArray(len, elem) =>
      Seq(elem) ++ len.asV
    case Store(addr, index, value, _) =>
      Seq(addr, value) ++ index.asV
    case Load(addr, index, _) =>
      Seq(addr) ++ index.asV
    case UnOpTree(_, value) =>
      Seq(value)
    case BinOpVCTree(_, left, right) =>
      Seq(left) ++ right.asV
    case BinOpVTree(_, left, right) =>
      Seq(left, right)
    case Select(cond, tru, fls) =>
      Seq(cond.left, tru, fls) ++ cond.rightVC.asV
    case Write(value) =>
      Seq(value)
    case CallDir(_, args, None) =>
      args
  }

  def toBriefString: String = toString
}

final case class Mv(id: XID) extends Instruction

final case class Mvi(value: Word) extends Instruction

object Mvi {
  def int(i: Int) = Mvi(Word(i))

  def float(f: Float) = Mvi(Word.fromFloat(f))
}

final case class NewArray(len: VC, elem: XID) extends Instruction

final case class Store(addr: XID, index: C, value: XID, originalIndex: MemoryIndex)
  extends Instruction

final case class Load(addr: XID, index: VC, originalIndex: MemoryIndex) extends Instruction

final case class UnOpTree(op: UnOp, value: XID) extends Instruction

final case class BinOpVCTree(op: BinOpVC, left: XID, right: VC) extends Instruction

final case class BinOpVTree(op: BinOpV, left: XID, right: XID) extends Instruction

final case class Select(cond: Branch.Cond, tru: XID, fls: XID) extends Instruction

case object Nop extends Instruction

case object Read extends Instruction

final case class Write(value: XID) extends Instruction

final case class CallDir(fn: String, args: List[XID], saves: Option[Map[ID, XReg]])
  extends Instruction {
  override def toBriefString =
    s"CallDir($fn; ${if (args.isEmpty) "Nil" else args.mkString(", ")})"
}
