package net.akouryy.anscaml
package knorm

import base._
import syntax.{BinOp, CmpOp}

final case class KNorm(comment: Comment, raw: KNorm.Raw) {
  def +(newComment: Comment) = KNorm(newComment + comment, raw)
}

object KNorm {

  def apply(raw: KNorm.Raw): KNorm = KNorm(NoComment, raw)

  final case class FDef(entry: Entry, args: List[Entry], body: KNorm, noInline: Boolean)

  sealed trait Raw

  final case class KInt(i: Int) extends Raw

  final case class KFloat(f: Float) extends Raw

  final case class BinOpTree[T <: Primitives.IF](op: BinOp[T], left: ID, right: ID) extends Raw

  final case class IfCmp[T <: Primitives.IF](
    op: CmpOp[T], left: ID, right: ID, tru: KNorm, fls: KNorm
  ) extends Raw

  final case class Var(v: ID) extends Raw

  final case class App(fn: ID, args: List[ID]) extends Raw

  final case class KTuple(elems: List[ID]) extends Raw

  final case class Get(array: ID, index: ID) extends Raw

  final case class Put(array: ID, index: ID, value: ID) extends Raw

  final case class ExtArray(array: ID) extends Raw

  final case class ExtFunApp(fn: ID, args: List[ID]) extends Raw

  final case class Let(entry: Entry, bound: KNorm, kont: KNorm) extends Raw

  final case class LetTuple(elems: List[Entry], bound: ID, kont: KNorm) extends Raw

  final case class LetRec(fDef: KNorm.FDef, kont: KNorm) extends Raw

}
