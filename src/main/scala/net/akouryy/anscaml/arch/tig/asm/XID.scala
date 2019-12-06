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
  def generate(str: String) = XVar(ID.generate(ID(str)))

  def generate(xid: XID, suffix: String = "") =
    XVar(ID.generate(ID(s"${xid.idStr}$$$suffix")))
}

final case class XReg(id: Int) extends XID {

  import XReg._

  assert(-1 <= id && id < REG_SIZE)

  override val idStr: String = if (id == -1) "$reg_x" else s"$$reg$id"

  override def toString: String = if (id == -1) "%rx" else s"%r$id"
}

object XReg {
  val REG_SIZE = 32

  val VALID_REGS: IndexedSeq[XReg] = (-1 until REG_SIZE).map(XReg.apply)
  val NORMAL_REGS: IndexedSeq[XReg] = (1 to 25).map(XReg.apply)

  val REG_DUMMY = XReg(-1)
  val REG_ZERO = XReg(0)
  val REG_RETURN = XReg(1)
  val REG_ONE = XReg(26)
  val REG_MINUS_ONE = XReg(27)
  val REG_LAST_TMP = XReg(28)
  val REG_HEAP = XReg(29)
  val REG_STACK = XReg(30)
  val REG_LINK = XReg(31)

  val fromConstants: Map[Word, XReg] = Map(
    Word.fromInt(0) -> REG_ZERO,
    Word.fromInt(1) -> REG_ONE,
    Word.fromInt(-1) -> REG_MINUS_ONE,
  )

  val toConstants: Map[XReg, Word] = fromConstants.map(_.swap)
}
