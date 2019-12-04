package net.akouryy.anscaml
package arch.tig.asm

import base._

final case class AEntry(aid: AID, ty: Ty)

sealed trait AID {
  val idStr: String

  def aVarOpt: Option[AVar] = this match {
    case v: AVar => Some(v)
    case _: AReg => None
  }

}

final case class AVar(id: ID) extends AID {
  override val idStr: String = id.str

  override def toString: String = id.str
}

object AVar {
  def generate(str: String) = AVar(ID.generate(ID(str)))

  def generate(aid: AID, suffix: String = "") =
    AVar(ID.generate(ID(s"${aid.idStr}$$$suffix")))
}

final case class AReg(id: Int) extends AID {

  import AReg._

  assert(-1 <= id && id < REG_SIZE)

  override val idStr: String = if (id == -1) "$reg_x" else s"$$reg$id"

  override def toString: String = if (id == -1) "%rx" else s"%r$id"
}

object AReg {
  val REG_SIZE = 32

  val NORMAL_REG_IDS: Array[Int] = (1 to 26).toArray

  val REG_DUMMY = AReg(-1)
  val REG_ZERO = AReg(0)
  val REG_ONE = AReg(26)
  val REG_MINUS_ONE = AReg(27)
  val REG_TMP = AReg(28)
  val REG_HEAP = AReg(29)
  val REG_STACK = AReg(30)
  val REG_LINK = AReg(31)

  val fromConstants: Map[Int, AReg] = Map(
    0 -> REG_ZERO,
    1 -> REG_ONE,
    -1 -> REG_MINUS_ONE,
  )

  val toConstants: Map[AReg, Int] = fromConstants.map(_.swap)
}
