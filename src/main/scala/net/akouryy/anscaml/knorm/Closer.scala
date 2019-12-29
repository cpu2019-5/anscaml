package net.akouryy.anscaml
package knorm

import base._
import KNorm._

import scala.collection.mutable

class Closer {
  private[this] val directFunctions = mutable.Set[ID]()
  private[this] var globalConstsRev = List[(Entry, KClosed)]()
  private[this] val globalConstNames = mutable.Set[ID]()
  private[this] var globalFunctionsRev = List[CFDef]()

  private[this] sealed trait Scope

  private[this] case object Local extends Scope

  private[this] case object Global extends Scope

  def apply(kn: KNorm): KCProgram = {
    Logger.log("KC", "Start")
    directFunctions.clear()
    globalConstsRev = Nil
    globalConstNames.clear()
    indirectFVsCache.clear()
    val cl = close(kn, Global)
    KCProgram(globalConstsRev.reverse, globalFunctionsRev.reverse, cl)
  }

  private[this] val indirectFVsCache = mutable.Map[ID, Set[ID]]()

  private[this] def indirectFVs(body: KNorm, localFns: Set[ID]): Set[ID] = {
    body.raw match {
      case KInt(_) | KFloat(_) => Set()
      case BinOpTree(_, x, y) => Set(x, y)
      case Var(v) => Set(v)
      case KTuple(elems) => elems.toSet
      case KArray(x, y) => Set(x, y)
      case Get(x, y) => Set(x, y)
      case Put(x, y, z) => Set(x, y, z)
      case Apply(fn, args, _) => Set(fn) &~ directFunctions &~ localFns | args.toSet
      case ApplyExternal(_, args) => args.toSet
      case IfCmp(_, x, y, tru, fls) =>
        Set(x, y) | indirectFVs(tru, localFns) | indirectFVs(fls, localFns)
      case Let(entry, bound, kont) =>
        indirectFVsCache.getOrElseUpdate(entry.name,
          indirectFVs(kont, localFns) - entry.name | indirectFVs(bound, localFns)
        )
      case LetTuple(elems, bound, kont) =>
        indirectFVs(kont, localFns) &~ elems.map(_.name).toSet | Set(bound)
      case LetRec(FDef(Entry(name, _), args, body, _), kont) =>
        val lf = localFns + name
        indirectFVsCache.getOrElseUpdate(name,
          indirectFVs(body, lf) &~ args.map(_.name).toSet | indirectFVs(kont, lf)
        )
    }
  }

  private[this] def close(norm: KNorm, scope: Scope): KClosed =
    norm.raw match {
      case raw: KCRaw => KClosed(norm.comment, raw)
      case Apply(fn, args, _) =>
        if (directFunctions contains fn) {
          KClosed(norm.comment, ApplyDirect(fn.str, args))
        } else {
          KClosed(norm.comment, ApplyClosure(fn, args))
        }
      case ApplyExternal(fn, args) =>
        KClosed(norm.comment, ApplyDirect(ID.Special.EXTERNAL_PREFIX +! fn.str, args))
      case IfCmp(op, left, right, tru, fls) =>
        KClosed(norm.comment, CIfCmp(op, left, right, close(tru, Local), close(fls, Local)))
      case Let(entry, bound, kont) =>
        if (scope == Global && !bound.raw.mayHaveEffect) {
          globalConstsRev ::= (entry, close(bound, Local))
          globalConstNames += entry.name
          close(kont, Global)
        } else {
          if (scope == Global) Logger.log("KC", s"may mutate: ${entry.name}")
          KClosed(norm.comment, CLet(entry, close(bound, Local), close(kont, Local)))
        }
      case LetTuple(elems, bound, kont) =>
        KClosed(norm.comment, CLetTuple(elems, bound, close(kont, scope)))
      case LetRec(FDef(entry @ Entry(id @ ID(name), _), args, body, _), kont) =>
        val bodyFVs = (
          indirectFVs(body, Set(id)) -- args.map(_.name).toSet -- globalConstNames
          ).toList
        if (bodyFVs.isEmpty) {
          directFunctions += id
        }
        val kontFVs = indirectFVs(kont, Set()) -- globalConstNames
        val bodyKC = close(body, Local)
        val kontKC = close(kont, scope)
        globalFunctionsRev ::= CFDef(entry, args, bodyFVs, bodyKC)

        if (kontFVs contains id) {
          // kontでこの関数のクロージャを用いる
          Logger.log("KC", s"closure for $name with fvs $bodyFVs")
          KClosed(norm.comment, CLetClosure(entry, LabelID(name), bodyFVs, kontKC))
        } else {
          // println(s"[KNorm Closer] no closure for $name")
          kontKC
        }
    }
}
