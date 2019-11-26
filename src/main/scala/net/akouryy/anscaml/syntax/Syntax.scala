package net.akouryy.anscaml
package syntax

import base._

sealed abstract class Syntax

object Syntax {

  case class FDef(entry: Entry, args: List[Entry], body: Syntax, noInline: Boolean)

  // ASTs

  case object Unit extends Syntax

  case class LitBool(b: Boolean) extends Syntax

  case class LitInt(i: Int) extends Syntax

  case class LitFloat(f: Float) extends Syntax

  case class Not(s: Syntax) extends Syntax

  case class Neg(s: Syntax) extends Syntax

  case class BinOpTree[T <: IntOrFloat](op: BinOp[T], left: Syntax, right: Syntax) extends Syntax

  case class Fneg(s: Syntax) extends Syntax

  case class CmpOpTree[T <: IntOrFloat](op: CmpOp[T], left: Syntax, right: Syntax) extends Syntax

  case class If(cond: Syntax, tru: Syntax, fls: Syntax) extends Syntax

  case class Let(entry: Entry, bound: Syntax, kont: Syntax) extends Syntax

  case class Var(name: ID) extends Syntax

  case class LetRec(fn: FDef, kont: Syntax) extends Syntax

  case class Apply(fn: Syntax, args: List[Syntax]) extends Syntax

  case class Tuple(elems: List[Syntax]) extends Syntax

  case class LetTuple(elems: List[Entry], bound: Syntax, kont: Syntax) extends Syntax

  case class Array(length: Syntax, elem: Syntax) extends Syntax

  case class Get(array: Syntax, index: Syntax) extends Syntax

  case class Put(array: Syntax, index: Syntax, value: Syntax) extends Syntax

}
