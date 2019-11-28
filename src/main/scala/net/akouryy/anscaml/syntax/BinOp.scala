package net.akouryy.anscaml
package syntax

import base._

sealed abstract class BinOp[IF <: Primitives.IF](val fn: (IF#T, IF#T) => IF#T)(
  implicit val prim: IF
) {
  //  val prim: Primitives.IntOrFloat = primRaw
}

object BinOp {

  case object Add extends BinOp[Primitives.PInt]((x, y) => x + y)

  case object Sub extends BinOp[Primitives.PInt]((x, y) => x - y)

  case object Mul extends BinOp[Primitives.PInt]((x, y) => x * y)

  case object Div extends BinOp[Primitives.PInt]((x, y) => x / y)

  case object Mod extends BinOp[Primitives.PInt]((x, y) => x % y)

  case object Shl extends BinOp[Primitives.PInt]((x, y) => x << y)

  case object Shr extends BinOp[Primitives.PInt]((x, y) => x >> y)

  case object Fadd extends BinOp[Primitives.PFloat]((x, y) => x + y)

  case object Fsub extends BinOp[Primitives.PFloat]((x, y) => x - y)

  case object Fmul extends BinOp[Primitives.PFloat]((x, y) => x * y)

  case object Fdiv extends BinOp[Primitives.PFloat]((x, y) => x / y)

}
