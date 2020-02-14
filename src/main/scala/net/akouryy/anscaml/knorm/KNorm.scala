package net.akouryy.anscaml
package knorm

import base._
import syntax.{BinOp, CmpOp}

final case class KNorm(comment: Comment, raw: KNorm.KRaw) {
  def +(newComment: Comment) = KNorm(newComment + comment, raw)

  def replaceVars(map: Map[ID, ID]): KNorm = {
    import shapeless._
    object inc extends Poly1 {
      //noinspection TypeAnnotation
      implicit def atID = at[ID](x => map.getOrElse(x, x))
    }
    // shapelessの?バグのため一時変数に代入
    val res = everywhere(inc)(this)
    res
  }
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
      case ForCmp(_, _, _, _, _, _, body, kont) => body.raw.mayHaveEffect || kont.raw.mayHaveEffect
      case Let(_, bound, kont) => bound.raw.mayHaveEffect || kont.raw.mayHaveEffect
      case LetTuple(_, _, kont) => kont.raw.mayHaveEffect
      case LetRec(_, kont) => kont.raw.mayHaveEffect
      case _: Apply | _: Put | _: ApplyExternal => true
      case _ => false
    }
  }

  sealed trait HasKont extends KRaw {
    def kont: KNorm

    def copyWithKont(kont: KNorm): KRaw
  }

  /** K-normalized closure raw */
  sealed trait KCRaw {}

  final case class KInt(i: Int) extends KRaw with KCRaw

  final case class KFloat(f: Float) extends KRaw with KCRaw

  final case class BinOpTree(op: BinOp, left: ID, right: ID) extends KRaw with KCRaw

  final case class Var(v: ID) extends KRaw with KCRaw

  final case class KTuple(elems: List[ID]) extends KRaw with KCRaw

  final case class ForUpdater(elems: List[ID]) extends KRaw with KCRaw

  final case class KArray(len: ID, elem: ID) extends KRaw with KCRaw

  final case class Get(array: ID, index: ID) extends KRaw with KCRaw

  final case class Put(array: ID, index: ID, value: ID) extends KRaw with KCRaw

  final case class Apply(fn: ID, args: List[ID], info: ApplyInfo) extends KRaw

  final case class ApplyInfo(isRec: Boolean, isTail: Boolean)

  final case class ApplyExternal(fn: ID, args: List[ID]) extends KRaw

  final case class ApplyDirect(fn: String, args: List[ID]) extends KCRaw

  final case class ApplyClosure(fn: ID, args: List[ID]) extends KCRaw

  final case class IfCmp(op: CmpOp, left: ID, right: ID, tru: KNorm, fls: KNorm) extends KRaw

  /**
    * @see net.akouryy.anscaml.knorm.optimize.LoopDetector
    * @param negated  true if the loop should run <em>UNTIL</em> the comparison holds
    * @param initVars variables holding initial values of `loopVars`
    * @param body     the loop body to return a tuple of values of `loopVars` in the next iteration
    */
  final case class ForCmp(
    op: CmpOp, left: ID, right: ID, negated: Boolean,
    loopVars: List[ID], initVars: List[ID], body: KNorm, kont: KNorm
  ) extends KRaw with HasKont {
    override def copyWithKont(kont: KNorm): KRaw = copy(kont = kont)

    def mapBodyKont(b: KNorm => KNorm)(k: KNorm => KNorm): ForCmp =
      copy(body = b(body), kont = k(kont))
  }

  final case class Let(entry: Entry, bound: KNorm, kont: KNorm) extends KRaw with HasKont {
    override def copyWithKont(kont: KNorm): KRaw = copy(kont = kont)
  }

  final case class LetTuple(elems: List[Entry], bound: ID, kont: KNorm) extends KRaw with HasKont {
    override def copyWithKont(kont: KNorm): KRaw = copy(kont = kont)
  }

  final case class LetRec(fDef: KNorm.FDef, kont: KNorm) extends KRaw with HasKont {
    override def copyWithKont(kont: KNorm): KRaw = copy(kont = kont)
  }

  final case class CIfCmp(op: CmpOp, left: ID, right: ID, tru: KClosed, fls: KClosed)
    extends KCRaw

  final case class CForCmp(
    op: CmpOp, left: ID, right: ID, negated: Boolean,
    loopVars: List[ID], initVars: List[ID], body: KClosed, kont: KClosed
  ) extends KCRaw

  final case class CLet(entry: Entry, bound: KClosed, kont: KClosed) extends KCRaw

  final case class CLetTuple(elems: List[Entry], bound: ID, kont: KClosed) extends KCRaw

  final case class CLetClosure(entry: Entry, fn: LabelID, captured: List[ID], kont: KClosed)
    extends KCRaw

}
