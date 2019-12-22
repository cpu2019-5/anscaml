package net.akouryy.anscaml
package arch.tig.asm

import base._

import scala.collection.immutable

sealed trait XID extends Any {
  def idStr: String

  final def asXVar: Option[XVar] = this match {
    case v: XVar => Some(v)
    case _: XReg => None
  }

  final def asXReg: Option[XReg] = this match {
    case r: XReg => Some(r)
    case _: XVar => None
  }

  final def fold[T](ifVar: XVar => T, ifReg: XReg => T): T = this match {
    case v: XVar => ifVar(v)
    case r: XReg => ifReg(r)
  }
}

final case class XVar(idStr: String) extends AnyVal with XID {
  override def toString: String = idStr
}

object XVar {
  def generate(str: String, allowEmptySuffix: Boolean = false) =
    XVar(ID.generate(str, allowEmptySuffix).str)
}

final case class XReg private(id: Int) extends AnyVal with XID with Ordered[XReg] {
  override def idStr: String = if (id == -1) "$reg_x" else s"$$reg$id"

  override def toString: String = if (id == -1) "%rx" else s"%r$id"

  override def compare(that: XReg): Int = id compare that.id
}

object XReg {
  def apply(id: Int): XReg = {
    assert(-1 <= id && id < REG_SIZE)
    new XReg(id)
  }

  val REG_SIZE = 64

  val VALID_REGS: IndexedSeq[XReg] = -1 until REG_SIZE map XReg.apply
  val NORMAL_REGS: IndexedSeq[XReg] = (1 to 27) ++ (32 to 60) map XReg.apply

  val NORMAL_REGS_SET: immutable.SortedSet[XReg] = NORMAL_REGS.to(immutable.SortedSet)

  val DUMMY = XReg(-1)
  val ZERO = XReg(0)
  val RETURN = XReg(1)
  val LAST_TMP = XReg(28)
  val HEAP = XReg(29)
  val STACK = XReg(30)
  val LINK = XReg(31)
  val C_255 = XReg(61)
  val C_ONE = XReg(62)
  val C_MINUS_ONE = XReg(63)

  val fromConstants: Map[Word, XReg] = Map(
    Word(0) -> ZERO,
    Word(1) -> C_ONE,
    Word(-1) -> C_MINUS_ONE,
    Word(255) -> C_255,
  )

  val toConstants: Map[XReg, Word] = fromConstants.map(_.swap)

  def toRangeString(rs: Iterable[XReg]): String = {
    var start: Option[Int] = None
    var last: Option[Int] = None
    val sb = new StringBuilder()
    for (XReg(i) <- rs) {
      if (!last.contains(i - 1)) {
        for (j <- start) {
          val k = last.get
          sb ++= (if (j == k) s"r$j," else s"r$j-r$k,")
        }
        start = Some(i)
      }
      last = Some(i)
    }
    for (j <- start) {
      val k = last.get
      sb ++= (if (j == k) s"r$j" else s"r$j-r$k")
    }
    s"XRegs($sb)"
  }
}
