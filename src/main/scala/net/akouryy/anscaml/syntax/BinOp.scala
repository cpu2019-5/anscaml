package net.akouryy.anscaml
package syntax

import base._

sealed abstract class BinOp[IF <: IntOrFloat](fn: (IF#T, IF#T) => IF#T)

object BinOp {

  case object Add extends BinOp[IntOrFloat.Int]((x, y) => x + y)

  case object Sub extends BinOp[IntOrFloat.Int]((x, y) => x - y)

  case object Mul extends BinOp[IntOrFloat.Int]((x, y) => x * y)

  case object Div extends BinOp[IntOrFloat.Int]((x, y) => x / y)

  case object Mod extends BinOp[IntOrFloat.Int]((x, y) => x % y)

  case object Shl extends BinOp[IntOrFloat.Int]((x, y) => x << y)

  case object Shr extends BinOp[IntOrFloat.Int]((x, y) => x >> y)

  case object Fadd extends BinOp[IntOrFloat.Float]((x, y) => x + y)

  case object Fsub extends BinOp[IntOrFloat.Float]((x, y) => x - y)

  case object Fmul extends BinOp[IntOrFloat.Float]((x, y) => x * y)

  case object Fdiv extends BinOp[IntOrFloat.Float]((x, y) => x / y)

}
