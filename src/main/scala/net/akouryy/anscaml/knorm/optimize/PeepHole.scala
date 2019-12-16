package net.akouryy.anscaml
package knorm
package optimize

import base._
import syntax.{BinOp, CmpOp}
import typ.{Lit, Typ}
import KNorm._

import scala.collection.mutable
import scala.util.control.TailCalls._

class PeepHole {
  private[this] val env = mutable.Map[ID, KRaw]()

  def apply(norm: KNorm): KNorm = {
    Logger.log("KO-PH", "Start")
    env.clear()
    optimize(norm).result
  }

  def optimize(norm: KNorm): TailRec[KNorm] = {
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
        done(env.get(v) match {
          case Some(r @ (KInt(_) | KFloat(_))) => addCmt("lit", r)
          case _ => norm
        })
      case BinOpTree(op, left, right) =>
        done((op, env.get(left), env.get(right)) match {
          case (BinOp.III(fn), Some(KInt(x)), Some(KInt(y))) =>
            addCmt(s"op-$op", KInt(fn(x, y)))
          case (BinOp.FFF(fn), Some(KFloat(x)), Some(KFloat(y))) =>
            addCmt(s"op-$op", KFloat(fn(x, y)))
          case (BinOp.Mul, _, Some(KInt(y))) if Util.log2(y).nonEmpty =>
            val z = ID.generate(ID(s"${right.str}$$log"))
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
            val z = ID.generate(ID(s"${right.str}$$log"))
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
            val z = ID.generate(ID(s"${right.str}$$log"))
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
        })
      case IfCmp(op, left, right, tru, fls) =>
        (op, env.get(left), env.get(right)) match {
          case (CmpOp.Le, Some(BinOpTree(BinOp.Sub, minusOne, x)), Some(KInt(-1)))
            if env.get(minusOne).contains(KInt(-1)) =>
            done(addCmt("if-not", IfCmp(CmpOp.Eq, x, right /* -1 */ , fls, tru)))
          case (
            CmpOp.Le,
            Some(IfCmp(op2, l2, r2, KNorm(_, KInt(-1)), KNorm(_, KInt(0)))),
            Some(KInt(-1))
            ) =>
            // `if [(l2 <=> r2) <= -1]` -> `if [l2 <=> r2]`
            done(addCmt("if-cmp-cmp", IfCmp(op2, l2, r2, tru, fls)))
          case (CmpOp.II(fn), Some(KInt(x)), Some(KInt(y))) =>
            if (fn(x, y))
              optimize(tru).map(addCmtNorm("if-true", _))
            else
              optimize(fls).map(addCmtNorm("if-false", _))
          case (CmpOp.FF(fn), Some(KFloat(x)), Some(KFloat(y))) =>
            if (fn(x, y))
              optimize(tru).map(addCmtNorm("if-true", _))
            else
              optimize(fls).map(addCmtNorm("if-false", _))
          case _ =>
            for {
              t <- optimize(tru)
              f <- optimize(fls)
            } yield {
              norm.copy(raw = IfCmp(op, left, right, t, f))
            }
        }

      case Let(Entry(name, typ), bound, kont) =>
        for {
          boundOpt <- optimize(bound)
          () = env(name) = boundOpt.raw
          k <- optimize(kont)
        } yield {
          val newTyp = boundOpt.raw match {
            case KInt(i) => Typ.IntList(i)
            case KFloat(f) => Typ.FloatList(f)
            case _ => typ
          }
          norm.copy(raw = Let(Entry(name, newTyp), boundOpt, k))
        }
      case LetTuple(elems, bound, kont) =>
        // 面倒なのでenvは更新しない
        for (k <- optimize(kont)) yield {
          norm.copy(raw = LetTuple(elems, bound, k))
        }
      case LetRec(FDef(entry, args, body, noInline), kont) =>
        // 関数はenvに含めない
        for {
          b <- optimize(body)
          k <- optimize(kont)
        } yield {
          norm.copy(raw = LetRec(FDef(entry, args, b, noInline), k))
        }
      case _ => done(norm)
    }
  }

}
