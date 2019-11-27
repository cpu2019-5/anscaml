package net.akouryy.anscaml
package syntax

import base._
import typ.Typ

sealed abstract class BinOp[IF <: Primitives.IntOrFloat](val fn: (IF#T, IF#T) => IF#T)(
  implicit val prim: IF
) {
  //  val prim: Primitives.IntOrFloat = primRaw
}

object BinOp {

  case object Add extends BinOp[Primitives.Int]((x, y) => x + y)

  case object Sub extends BinOp[Primitives.Int]((x, y) => x - y)

  case object Mul extends BinOp[Primitives.Int]((x, y) => x * y)

  case object Div extends BinOp[Primitives.Int]((x, y) => x / y)

  case object Mod extends BinOp[Primitives.Int]((x, y) => x % y)

  case object Shl extends BinOp[Primitives.Int]((x, y) => x << y)

  case object Shr extends BinOp[Primitives.Int]((x, y) => x >> y)

  case object Fadd extends BinOp[Primitives.Float]((x, y) => x + y)

  case object Fsub extends BinOp[Primitives.Float]((x, y) => x - y)

  case object Fmul extends BinOp[Primitives.Float]((x, y) => x * y)

  case object Fdiv extends BinOp[Primitives.Float]((x, y) => x / y)

}
