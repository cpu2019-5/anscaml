package net.akouryy.anscaml
package syntax

import base._

sealed abstract class CmpOp[IF <: Primitives.IntOrFloat](fn: (IF#T, IF#T) => Boolean)(
  implicit val prim: IF
)

object CmpOp {

  case object Eq extends CmpOp[Primitives.Int]((x, y) => x == y)

  case object Le extends CmpOp[Primitives.Int]((x, y) => x < y)

  case object Feq extends CmpOp[Primitives.Float]((x, y) => x == y)

  case object Fle extends CmpOp[Primitives.Float]((x, y) => x < y)

}
