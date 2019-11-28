package net.akouryy.anscaml
package syntax

import base._

sealed abstract class CmpOp[IF <: Primitives.IF](val fn: (IF#T, IF#T) => Boolean)(
  implicit val prim: IF
)

object CmpOp {

  case object Eq extends CmpOp[Primitives.PInt]((x, y) => x == y)

  case object Le extends CmpOp[Primitives.PInt]((x, y) => x < y)

  case object Feq extends CmpOp[Primitives.PFloat]((x, y) => x == y)

  case object Fle extends CmpOp[Primitives.PFloat]((x, y) => x < y)

}
