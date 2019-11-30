package net.akouryy.anscaml
package knorm
package optimize

import base._
import typ.{Lit, Typ}
import KNorm._

import scala.collection.mutable

class TypFolder {
  private[this] val typEnv = mutable.Map[ID, Typ]()

  def apply(kn: KNorm): KNorm = {
    println("[KNorm TypFolder] Start")
    fold(kn)
  }

  private[this] def fold(kn: KNorm): KNorm =
    kn.copy(raw = kn.raw match {
      case IfCmp(op, left, right, tru, fls) =>
        IfCmp(op, left, right, fold(tru), fold(fls))
      case Let(entry, bound, kont) =>
        typEnv += entry.toPair
        Let(entry, fold(bound), fold(kont))
      case LetTuple(elems, bound, kont) =>
        typEnv ++= elems.map(_.toPair)
        LetTuple(elems, bound, fold(kont))
      case LetRec(FDef(entry, args, body, noInline), kont) =>
        typEnv ++= entry.toPair :: args.map(_.toPair)
        LetRec(FDef(entry, args, fold(body), noInline), fold(kont))

      case App(fn, List(arg)) =>
        typEnv.get(arg) match {
          case Some(Typ.TInt(Lit.List(s))) if s.sizeIs >= 2 =>
            println(s"[KNorm TypFolder] fold ${fn.name} for $arg in {${s.mkString(",")}}")
            // TODO: 二分探索?
            s.foldLeft[Option[KNorm.Raw]](None) {
              case (None, lit) =>
                val a = ID.generate(arg)
                Some(Let(
                  Entry(a, Typ.TInt(Lit.List[Primitives.PInt](Set(lit)))),
                  KNorm(KInt(lit)),
                  KNorm(
                    Commented(s"[KNorm TypFolder] fold ${fn.name}($arg = $lit)"),
                    App(fn, List(a))
                  )
                ))
              case (Some(otherApplies), lit) =>
                val a = ID.generate(arg)
                Some(Let(
                  Entry(a, Typ.TInt(Lit.List[Primitives.PInt](Set(lit)))),
                  KNorm(KInt(lit)),
                  KNorm(
                    Commented(s"[KNorm TypFolder] fold ${fn.name}($arg = $lit)"),
                    IfCmp(
                      syntax.CmpOp.Eq, arg, a,
                      KNorm(App(fn, List(a))),
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
