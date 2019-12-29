package net.akouryy.anscaml
package knorm
package optimize

import base._
import syntax.{BinOp, CmpOp}
import typ.{Lit, Typ}
import KNorm._

import scala.collection.mutable

class PeepHole {
  private[this] val env = mutable.Map[ID, KRaw]()

  def apply(norm: KNorm): KNorm = {
    Logger.log("KO-PH", "Start")
    env.clear()
    optimize(norm)
  }

  def optimize(norm: KNorm): KNorm = {
    def addCmt(msg: String, raw: KRaw) = KNorm(
      norm.comment :+ s"[KNorm PeepHole] $msg",
      raw
    )

    def addCmtNorm(msg: String, norm2: KNorm) = KNorm(
      norm2.comment :+ s"[KO PeepHole] $msg",
      norm2.raw
    )

    norm.raw match {
      case Var(v) =>
        env.get(v) match {
          case Some(r @ (KInt(_) | KFloat(_))) => addCmt("lit", r)
          case _ => norm
        }
      case BinOpTree(op, left, right) =>
        (op, env.get(left), env.get(right)) match {
          case (BinOp.III(fn), Some(KInt(x)), Some(KInt(y))) =>
            addCmt(s"op-$op", KInt(fn(x, y)))
          case (BinOp.FFF(fn), Some(KFloat(x)), Some(KFloat(y))) =>
            addCmt(s"op-$op", KFloat(fn(x, y)))
          case (BinOp.Mul, _, Some(KInt(y))) if Util.log2(y).nonEmpty =>
            val z = ID.generate(s"${right.str}$$log")
            val log2 = Util.log2(y).get
            addCmt(
              "mul-to-shl",
              Let(
                Entry(z, Typ.TInt(Lit.List[Primitives.PInt](Set(log2)))),
                KNorm(KInt(log2)),
                KNorm(BinOpTree(BinOp.Shl, left, z)),
              ),
            )
          case (BinOp.Div, _, Some(KInt(y))) if Util.log2(y).nonEmpty =>
            val z = ID.generate(s"${right.str}$$log")
            val log2 = Util.log2(y).get
            addCmt(
              "div-to-shr",
              Let(
                Entry(z, Typ.TInt(Lit.List[Primitives.PInt](Set(log2)))),
                KNorm(KInt(log2)),
                KNorm(BinOpTree(BinOp.Shr, left, z)),
              ),
            )
          case (BinOp.Mod, _, Some(KInt(y))) if Util.log2(y).nonEmpty =>
            val z = ID.generate(s"${right.str}$$log")
            val log2 = Util.log2(y).get
            addCmt(
              "mod-to-land",
              Let(
                Entry(z, Typ.TInt(Lit.List[Primitives.PInt](Set(log2 - 1)))),
                KNorm(KInt(log2 - 1)),
                KNorm(BinOpTree(BinOp.Band, left, z)),
              ),
            )
          case _ => norm
        }
      case IfCmp(op, left, right, tru, fls) =>
        (op, env.get(left), env.get(right)) match {
          case (CmpOp.Le, Some(BinOpTree(BinOp.Sub, minusOne, x)), Some(KInt(-1)))
            if env.get(minusOne).contains(KInt(-1)) =>
            addCmt("if-not", IfCmp(CmpOp.Eq, x, right /* -1 */ , fls, tru))
          case (
            CmpOp.Le,
            Some(IfCmp(op2, l2, r2, KNorm(_, KInt(-1)), KNorm(_, KInt(0)))),
            Some(KInt(-1))
            ) =>
            // `if [(l2 <=> r2) <= -1]` -> `if [l2 <=> r2]`
            addCmt("if-cmp-cmp", IfCmp(op2, l2, r2, tru, fls))
          case (CmpOp.II(fn), Some(KInt(x)), Some(KInt(y))) =>
            if (fn(x, y))
              addCmtNorm("if-true", optimize(tru))
            else
              addCmtNorm("if-false", optimize(fls))
          case (CmpOp.FF(fn), Some(KFloat(x)), Some(KFloat(y))) =>
            if (fn(x, y))
              addCmtNorm("if-true", optimize(tru))
            else
              addCmtNorm("if-false", optimize(fls))
          case _ =>
            norm.copy(raw = IfCmp(op, left, right, optimize(tru), optimize(fls)))
        }

      case raw: ForCmp =>
        norm.copy(raw = raw.mapBodyKont(optimize)(optimize))
      case Let(Entry(name, typ), bound, kont) =>
        val boundOpt = optimize(bound)
        val newTyp = boundOpt.raw match {
          case KInt(i) => Typ.IntList(i)
          case KFloat(f) => Typ.FloatList(f)
          case _ => typ
        }
        env(name) = boundOpt.raw
        norm.copy(raw = Let(Entry(name, newTyp), boundOpt, optimize(kont)))
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
