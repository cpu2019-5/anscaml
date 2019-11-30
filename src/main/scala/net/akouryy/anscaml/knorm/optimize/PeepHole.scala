package net.akouryy.anscaml
package knorm
package optimize

import base._
import typ.{Lit, Typ}
import KNorm._
import net.akouryy.anscaml.syntax.{BinOp, CmpOp}

import scala.collection.mutable

class PeepHole {
  private[this] val env = mutable.Map[ID, KRaw]()

  def apply(norm: KNorm): KNorm = {
    println("[KNorm PeepHole] Start")
    env.clear()
    optimize(norm)
  }

  def optimize(norm: KNorm): KNorm = {
    def addCmt(raw: KRaw) = KNorm(
      norm.comment :+ s"[KNorm PeepHole] from $norm",
      raw
    )

    norm.raw match {
      case Var(v) =>
        env.get(v) match {
          case Some(r @ (KInt(_) | KFloat(_))) => addCmt(r)
          case _ => norm
        }
      case BinOpTree(op, left, right) =>
        (op, env.get(left), env.get(right)) match {
          case (BinOp.III(fn), Some(KInt(x)), Some(KInt(y))) =>
            addCmt(KInt(fn(x, y)))
          case (BinOp.FFF(fn), Some(KFloat(x)), Some(KFloat(y))) =>
            addCmt(KFloat(fn(x, y)))
          case (BinOp.Mul, _, Some(KInt(y))) if Util.log2(y).nonEmpty =>
            val z = ID.generate(ID(s"${y}Log"))
            val ans = Util.log2(y).get
            addCmt(Let(Entry(z, Typ.TInt(Lit.List[Primitives.PInt](Set(ans)))), KNorm(KInt(ans)),
              KNorm(BinOpTree(BinOp.Shl, left, z))))
          case _ => norm
        }
      case IfCmp(op, left, right, tru, fls) =>
        (op, env.get(left), env.get(right)) match {
          case (CmpOp.Eq, Some(BinOpTree(BinOp.Sub, one, x)), Some(KInt(0)))
            if env.get(one).contains(KInt(1)) => // TODO: EqやSubをパターンマッチで判定
            // Not
            addCmt(IfCmp(CmpOp.Eq, x, right /* zero */ , fls, tru))
          case (
            CmpOp.Eq,
            Some(IfCmp(op2, l2, r2, KNorm(_, KInt(1)), KNorm(_, KInt(0)))),
            Some(KInt(0))
            ) if op == CmpOp.Eq =>
            // `if [(l2 <=> r2) = 0] then tru else fls` -> `if [l2 <=> r2] then fls else tru`
            addCmt(IfCmp(op2, l2, r2, fls, tru))
          case (CmpOp.II(fn), Some(KInt(x)), Some(KInt(y))) =>
            if (fn(x, y))
              optimize(tru)
            else
              optimize(fls)
          case (CmpOp.FF(fn), Some(KFloat(x)), Some(KFloat(y))) =>
            if (fn(x, y))
              optimize(tru)
            else
              optimize(fls)
          case _ => norm.copy(raw = IfCmp(op, left, right, optimize(tru), optimize(fls)))
        }

      case Let(entry, bound, kont) =>
        val boundOpt = optimize(bound)
        env(entry.name) = boundOpt.raw
        norm.copy(raw = Let(entry, boundOpt, optimize(kont)))
      case LetTuple(elems, bound, kont) =>
        // 面倒なのでenvは更新しない
        norm.copy(raw = LetTuple(elems, bound, optimize(kont)))
      case LetRec(FDef(entry, args, body, noInline), kont) =>
        // 関数はenvに含めない
        norm.copy(raw = LetRec(FDef(entry, args, optimize(body), noInline), optimize(kont)))

      case _ => norm
    }
  }

}
