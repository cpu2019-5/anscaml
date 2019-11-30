package net.akouryy.anscaml
package knorm

import base._
import syntax.{BinOp, CmpOp}

final case class KNorm(comment: Comment, raw: KNorm.KRaw) {
  def +(newComment: Comment) = KNorm(newComment + comment, raw)
}

object KNorm {

  def apply(raw: KNorm.KRaw): KNorm = KNorm(NoComment, raw)

  final case class FDef(entry: Entry, args: List[Entry], body: KNorm, noInline: Boolean)

  sealed trait KRaw {
    def mayHaveEffect: Boolean = this match {
      case IfCmp(_, _, _, tru, fls) => tru.raw.mayHaveEffect || fls.raw.mayHaveEffect
      case Let(_, bound, kont) => bound.raw.mayHaveEffect || kont.raw.mayHaveEffect
      case LetTuple(_, _, kont) => kont.raw.mayHaveEffect
      case LetRec(_, kont) => kont.raw.mayHaveEffect
      case _: App | _: Put | _: ExtFunApp => true
      case _ => false
    }
  }

  final case class KInt(i: Int) extends KRaw

  final case class KFloat(f: Float) extends KRaw

  final case class BinOpTree[T <: Primitives.IF](op: BinOp[T], left: ID, right: ID) extends KRaw

  final case class Var(v: ID) extends KRaw

  final case class App(fn: ID, args: List[ID]) extends KRaw

  final case class KTuple(elems: List[ID]) extends KRaw

  final case class Get(array: ID, index: ID) extends KRaw

  final case class Put(array: ID, index: ID, value: ID) extends KRaw

  final case class ExtArray(array: ID) extends KRaw

  final case class ExtFunApp(fn: ID, args: List[ID]) extends KRaw

  final case class IfCmp[T <: Primitives.IF](
    op: CmpOp[T], left: ID, right: ID, tru: KNorm, fls: KNorm
  ) extends KRaw

  final case class Let(entry: Entry, bound: KNorm, kont: KNorm) extends KRaw

  final case class LetTuple(elems: List[Entry], bound: ID, kont: KNorm) extends KRaw

  final case class LetRec(fDef: KNorm.FDef, kont: KNorm) extends KRaw

}
