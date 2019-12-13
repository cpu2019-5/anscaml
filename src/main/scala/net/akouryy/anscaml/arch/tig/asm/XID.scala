package net.akouryy.anscaml
package arch.tig.asm

import base._

sealed trait XID {
  val idStr: String

  def asXVar: Option[XVar] = this match {
    case v: XVar => Some(v)
    case _: XReg => None
  }

  def asXReg: Option[XReg] = this match {
    case r: XReg => Some(r)
    case _: XVar => None
  }

  def fold[T](ifVar: XVar => T, ifReg: XReg => T): T = this match {
    case v: XVar => ifVar(v)
    case r: XReg => ifReg(r)
  }
}

final case class XVar(id: ID) extends XID {
  override val idStr: String = id.str

  override def toString: String = id.str
}

object XVar {
  def generate(str: String, allowEmptySuffix: Boolean = false) =
    XVar(ID.generate(ID(str), allowEmptySuffix))
}

final case class XReg(id: Int) extends XID {

  import XReg._

  assert(-1 <= id && id < REG_SIZE)

  override val idStr: String = if (id == -1) "$reg_x" else s"$$reg$id"

  override def toString: String = if (id == -1) "%rx" else s"%r$id"
}

object XReg {
  val REG_SIZE = 64

  val VALID_REGS: IndexedSeq[XReg] = -1 until REG_SIZE map XReg.apply
  val NORMAL_REGS: IndexedSeq[XReg] = (1 to 27) ++ (32 to 61) map XReg.apply

  val DUMMY = XReg(-1)
  val ZERO = XReg(0)
  val RETURN = XReg(1)
  val LAST_TMP = XReg(28)
  val HEAP = XReg(29)
  val STACK = XReg(30)
  val LINK = XReg(31)
  val C_ONE = XReg(62)
  val C_MINUS_ONE = XReg(63)

  val fromConstants: Map[Word, XReg] = Map(
    Word.fromInt(0) -> ZERO,
    Word.fromInt(1) -> C_ONE,
    Word.fromInt(-1) -> C_MINUS_ONE,
  )

  val toConstants: Map[XReg, Word] = fromConstants.map(_.swap)
}
