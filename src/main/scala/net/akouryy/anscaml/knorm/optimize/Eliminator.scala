package net.akouryy.anscaml
package knorm
package optimize

import base._
import KNorm._

object Eliminator {
  def apply(kn: KNorm): KNorm = {
    Logger.log("KO-EL", "Start")
    elim(kn)._1
  }

  /**
    * @return (除去後の式, 自由変数)
    */
  private[this] def elim(kn: KNorm): (KNorm, Set[ID]) = kn.raw match {
    case IfCmp(op, left, right, tru, fls) =>
      val (tk, tv) = elim(tru)
      val (fk, fv) = elim(fls)
      (kn.copy(raw = IfCmp(op, left, right, tk, fk)), tv | fv | Set(left, right))
    case LetTuple(elems, bound, kont) =>
      val (kk, kv) = elim(kont)
      (kn.copy(raw = LetTuple(elems, bound, kk)), kv -- elems.map(_.name) + bound)

    case Let(entry, bound, kont) =>
      val (bk, bv) = elim(bound)
      val (kk, kv) = elim(kont)
      if (kv.contains(entry.name) || bk.raw.mayHaveEffect) {
        (kn.copy(raw = Let(entry, bk, kk)), kv - entry.name | bv)
      } else {
        (KNorm(kn.comment + bk.comment + kk.comment, kk.raw), kv)
      }
    case LetRec(fDef, kont) =>
      val (bk, bv) = elim(fDef.body)
      val (kk, kv) = elim(kont)
      if (kv.contains(fDef.entry.name)) {
        (
          kn.copy(raw = LetRec(fDef.copy(body = bk), kk)),
          (bv | kv) -- (fDef.entry.name :: fDef.args.map(_.name))
        )
      } else {
        (KNorm(kn.comment + kk.comment, kk.raw), kv)
      }

    case _ => (kn, kn.raw match {
      case KInt(_) | KFloat(_) => Set()
      case BinOpTree(_, left, right) => Set(left, right)
      case Var(v) => Set(v)
      case Apply(fn, args, _) => (fn :: args).toSet
      case KTuple(elems) => elems.toSet
      case KArray(array, index) => Set(array, index)
      case Get(array, index) => Set(array, index)
      case Put(array, index, value) => Set(array, index, value)
      case ApplyExternal(_, args) => args.toSet
      case _ => ???
    })

  }
}
