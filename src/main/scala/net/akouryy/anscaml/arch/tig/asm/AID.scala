package net.akouryy.anscaml
package arch.tig.asm

import base._

final case class AEntry(aid: AID, ty: Ty)

sealed trait AID {
  val idStr: String
}

final case class AVar(id: ID) extends AID {
  override val idStr: String = id.name
}

object AVar {
  def generate(str: String) = AVar(ID.generate(ID(str)))

  def generate(aid: AID, suffix: String = "") =
    AVar(ID.generate(ID(s"${aid.idStr}$$$suffix")))
}

final case class AReg(id: Int) extends AID {

  import AReg._

  assert(-1 <= id && id < REG_SIZE)

  override val idStr: String = s"$$reg$id"

  override def toString: String = s"%r$id"
}

object AReg {
  val REG_SIZE = 32

  val NORMAL_REG_IDS: Array[Int] = (1 to 26).toArray

  val REG_DUMMY = AReg(-1)
  val REG_ZERO = AReg(0)
  val REG_MINUS1 = AReg(27)
  val REG_TMP = AReg(28)
  val REG_HEAP = AReg(29)
  val REG_STACK = AReg(30)
  val REG_LINK = AReg(31)
}
