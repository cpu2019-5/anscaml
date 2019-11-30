package net.akouryy.anscaml
package syntax

import typ.Typ

sealed trait CmpOp {
  val lhsTyp: Typ
  val rhsTyp: Typ
}

object CmpOp {

  sealed abstract case class II(val fn: (Int, Int) => Boolean) extends CmpOp {
    override val lhsTyp: Typ = Typ.IntAll
    override val rhsTyp: Typ = Typ.IntAll
  }

  sealed abstract case class FF(val fn: (Float, Float) => Boolean) extends CmpOp {
    override val lhsTyp: Typ = Typ.FloatAll
    override val rhsTyp: Typ = Typ.FloatAll
  }

  object Eq extends II((x, y) => x == y) {
    override def toString = "Eq"
  }

  object Le extends II((x, y) => x < y) {
    override def toString = "Le"
  }

  object Feq extends FF((x, y) => x == y) {
    override def toString = "Feq"
  }

  object Fle extends FF((x, y) => x < y) {
    override def toString = "Fle"
  }

}
