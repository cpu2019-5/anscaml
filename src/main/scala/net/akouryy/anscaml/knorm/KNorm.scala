package net.akouryy.anscaml
package knorm

import base._
import syntax.{BinOp, CmpOp}

final case class KNorm(comment: Comment, raw: KNorm.KRaw) {
  def +(newComment: Comment) = KNorm(newComment + comment, raw)
}

object KNorm {

  def apply(raw: KNorm.KRaw): KNorm = KNorm(NC, raw)

  final case class KCProgram(gConsts: List[(Entry, KClosed)], fDefs: List[CFDef], main: KClosed)

  final case class FDef(entry: Entry, args: List[Entry], body: KNorm, annot: Set[String])

  final case class CFDef(entry: Entry, args: List[Entry], fvs: List[ID], body: KClosed)

  final case class KClosed(comment: Comment, raw: KCRaw)

  sealed trait KRaw {
    def mayHaveEffect: Boolean = this match {
      case IfCmp(_, _, _, tru, fls) => tru.raw.mayHaveEffect || fls.raw.mayHaveEffect
      case Let(_, bound, kont) => bound.raw.mayHaveEffect || kont.raw.mayHaveEffect
      case LetTuple(_, _, kont) => kont.raw.mayHaveEffect
      case LetRec(_, kont) => kont.raw.mayHaveEffect
      case _: Apply | _: Put | _: ApplyExternal => true
      case _ => false
    }
  }

  /** K-normalized closure raw */
  sealed trait KCRaw {}

  final case class KInt(i: Int) extends KRaw with KCRaw

  final case class KFloat(f: Float) extends KRaw with KCRaw

  final case class BinOpTree(op: BinOp, left: ID, right: ID) extends KRaw with KCRaw

  final case class Var(v: ID) extends KRaw with KCRaw

  final case class KTuple(elems: List[ID]) extends KRaw with KCRaw

  final case class Array(len: ID, elem: ID) extends KRaw with KCRaw

  final case class Get(array: ID, index: ID) extends KRaw with KCRaw

  final case class Put(array: ID, index: ID, value: ID) extends KRaw with KCRaw

  final case class Apply(fn: ID, args: List[ID]) extends KRaw

  final case class ApplyExternal(fn: ID, args: List[ID]) extends KRaw

  final case class ApplyDirect(fn: String, args: List[ID]) extends KCRaw

  final case class ApplyClosure(fn: ID, args: List[ID]) extends KCRaw

  final case class IfCmp(op: CmpOp, left: ID, right: ID, tru: KNorm, fls: KNorm) extends KRaw

  final case class Let(entry: Entry, bound: KNorm, kont: KNorm) extends KRaw

  final case class LetTuple(elems: List[Entry], bound: ID, kont: KNorm) extends KRaw

  final case class LetRec(fDef: KNorm.FDef, kont: KNorm) extends KRaw

  final case class CIfCmp(op: CmpOp, left: ID, right: ID, tru: KClosed, fls: KClosed)
    extends KCRaw

  final case class CLet(entry: Entry, bound: KClosed, kont: KClosed) extends KCRaw

  final case class CLetTuple(elems: List[Entry], bound: ID, kont: KClosed) extends KCRaw

  final case class CLetClosure(entry: Entry, fn: LabelID, captured: List[ID], kont: KClosed)
    extends KCRaw

}
