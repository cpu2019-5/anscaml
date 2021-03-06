package net.akouryy.anscaml
package knorm
package optimize

import base._
import syntax.Annot
import typ.{Lit, Typ}
import KNorm._

import scala.collection.mutable

class TypFolder {
  private[this] val fnEnv = mutable.Map[ID, FDef]()
  private[this] val typEnv = mutable.Map[ID, Typ]()

  def apply(kn: KNorm): KNorm = {
    Logger.log("KO-TF", "Start")
    typEnv.clear()
    fold(kn)
  }

  private[this] def fold(kn: KNorm): KNorm =
    kn.copy(raw = kn.raw match {
      case IfCmp(op, left, right, tru, fls) =>
        IfCmp(op, left, right, fold(tru), fold(fls))
      case raw: ForCmp =>
        raw.mapBodyKont(fold)(fold)
      case Let(entry, bound, kont) =>
        typEnv += entry.toPair
        Let(entry, fold(bound), fold(kont))
      case LetTuple(elems, bound, kont) =>
        typEnv ++= elems.map(_.toPair)
        LetTuple(elems, bound, fold(kont))
      case LetRec(fDef @ FDef(entry, args, body, annot), kont) =>
        typEnv ++= entry.toPair :: args.map(_.toPair)
        if (annot.contains(Annot.TypFold)) fnEnv(entry.name) = fDef
        LetRec(FDef(entry, args, fold(body), annot), fold(kont))

      case Apply(fn, List(arg), _) =>
        (fnEnv.get(fn), typEnv.get(arg)) match {
          case (Some(fDef), Some(Typ.TInt(Lit.List(s)))) /*if s.sizeIs >= 2*/ =>
            Logger.log("KO-TF", s"fold ${fn.str} for ${arg.str} in {${
              s.mkString(",")
            }}")
            // TODO: 二分探索?
            s.foldLeft[Option[KNorm.KRaw]](None) {
              case (None, lit) =>
                val a = ID.generate(arg.str)
                val body = Alpha.convert(fDef.body, Map(fDef.args(0).name -> a))
                Some(Let(
                  Entry(a, Typ.TInt(Lit.List[Primitives.PInt](Set(lit)))),
                  KNorm(KInt(lit)),
                  KNorm(
                    body.comment :+ s"[KO TypFolder] fold ${fn.str}($arg = $lit)",
                    body.raw
                  )
                ))
              case (Some(otherApplies), lit) =>
                val a = ID.generate(arg.str)
                val body = Alpha.convert(fDef.body, Map(fDef.args(0).name -> a))
                Some(Let(
                  Entry(a, Typ.TInt(Lit.List[Primitives.PInt](Set(lit)))),
                  KNorm(KInt(lit)),
                  KNorm(
                    CM(s"[KO TypFolder] fold ${fn.str}($arg = $lit)"),
                    IfCmp(
                      syntax.CmpOp.Eq, arg, a,
                      body,
                      KNorm(otherApplies)
                    )
                  )
                ))
            }.get
          case _ => kn.raw
        }

      case _ => kn.raw
    })
}
