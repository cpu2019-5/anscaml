package net.akouryy.anscaml
package syntax

import base._

sealed trait Syntax {

  import Syntax._

  def recursively(convert: Syntax => Syntax): Syntax = this match {
    case LitBool(_) | LitInt(_) | LitFloat(_) | Var(_) => this
    case Not(s) => Not(convert(s))
    case BinOpTree(op, left, right) => BinOpTree(op, convert(left), convert(right))
    case CmpOpTree(op, left, right) => CmpOpTree(op, convert(left), convert(right))
    case If(cond, tru, fls) => If(convert(cond), convert(tru), convert(fls))
    case Let(entry, bound, kont) => Let(entry, convert(bound), convert(kont))
    case LetRec(FDef(entry, args, body, noInline), kont) =>
      LetRec(FDef(entry, args, convert(body), noInline), convert(kont))
    case Apply(fn, args) => Apply(convert(fn), args.map(convert))
    case Tuple(elems) => Tuple(elems.map(convert))
    case LetTuple(elems, bound, kont) => LetTuple(elems, convert(bound), convert(kont))
    case Array(length, elem) => Array(convert(length), convert(elem))
    case Get(array, index) => Get(convert(array), convert(index))
    case Put(array, index, value) => Put(convert(array), convert(index), convert(value))
  }
}

object Syntax {

  case class FDef(entry: Entry, args: List[Entry], body: Syntax, annot: Set[String])

  // ASTs

  val LitUnit = Tuple(Nil)

  case class LitBool(b: Boolean) extends Syntax

  case class LitInt(i: Int) extends Syntax

  case class LitFloat(f: Float) extends Syntax

  case class Not(s: Syntax) extends Syntax

  case class BinOpTree(op: BinOp, left: Syntax, right: Syntax) extends Syntax

  case class CmpOpTree(op: CmpOp, left: Syntax, right: Syntax) extends Syntax

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
