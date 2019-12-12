package net.akouryy.anscaml
package syntax

import typ.Typ

sealed trait BinOp {
  val lhsTyp: Typ
  val rhsTyp: Typ
  val retTyp: Typ
}

object BinOp {

  sealed abstract case class III(fn: (Int, Int) => Int) extends BinOp {
    override val lhsTyp: Typ = Typ.IntAll
    override val rhsTyp: Typ = Typ.IntAll
    override val retTyp: Typ = Typ.IntAll
  }

  sealed abstract case class FFF(fn: (Float, Float) => Float) extends BinOp {
    override val lhsTyp: Typ = Typ.FloatAll
    override val rhsTyp: Typ = Typ.FloatAll
    override val retTyp: Typ = Typ.FloatAll
  }

  object Add extends III((x, y) => x + y) {
    override def toString = "Add"
  }

  object Sub extends III((x, y) => x - y) {
    override def toString = "Sub"
  }

  object Mul extends III((x, y) => x * y) {
    override def toString = "Mul"
  }

  object Div extends III((x, y) => x / y) {
    override def toString = "Div"
  }

  object Mod extends III((x, y) => x % y) {
    override def toString = "Mod"
  }

  object Shl extends III((x, y) => x << y) {
    override def toString = "Shl"
  }

  object Shr extends III((x, y) => x >> y) {
    override def toString = "Shr"
  }

  object Band extends III((x, y) => x & y) {
    override def toString = "Land"
  }

  object Fadd extends FFF((x, y) => x + y) {
    override def toString = "Fadd"
  }

  object Fsub extends FFF((x, y) => x - y) {
    override def toString = "Fsub"
  }

  object Fmul extends FFF((x, y) => x * y) {
    override def toString = "Fmul"
  }

  object Fdiv extends FFF((x, y) => x / y) {
    override def toString = "Fdiv"
  }

}
