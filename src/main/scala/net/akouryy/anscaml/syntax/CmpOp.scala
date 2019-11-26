package net.akouryy.anscaml.syntax

import net.akouryy.anscaml.base.IntOrFloat

sealed abstract class CmpOp[IF <: IntOrFloat](fn: (IF#T, IF#T) => Boolean)

object CmpOp {

  case object Eq extends CmpOp[IntOrFloat.Int]((x, y) => x == y)

  case object Le extends CmpOp[IntOrFloat.Int]((x, y) => x < y)

  case object Feq extends CmpOp[IntOrFloat.Float]((x, y) => x == y)

  case object Fle extends CmpOp[IntOrFloat.Float]((x, y) => x < y)

}
