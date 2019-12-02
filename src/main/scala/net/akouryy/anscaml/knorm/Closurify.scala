package net.akouryy.anscaml
package knorm

import base._
import KNorm._

import scala.collection.mutable

class Closurify {
  private[this] val directFunctions = mutable.Set[ID]()
  private[this] var globalConstsRev = List[(Entry, KClosure)]()
  private[this] val globalConstNames = mutable.Set[ID]()
  private[this] var globalFunctionsRev = List[CFDef]()

  private[this] sealed trait Scope

  private[this] case object Local extends Scope

  private[this] case object Global extends Scope

  def apply(kn: KNorm): (List[(Entry, KClosure)], List[CFDef], KClosure) = {
    println("[KNorm Closurify] Start")
    directFunctions.clear()
    globalConstsRev = Nil
    globalConstNames.clear()
    val cl = closure(kn, Global)
    (globalConstsRev.reverse, globalFunctionsRev.reverse, cl)
  }

  private[this] def indirectFVs(body: KNorm, localFns: Set[ID]): Set[ID] = {
    body.raw match {
      case KInt(_) | KFloat(_) => Set()
      case BinOpTree(_, x, y) => Set(x, y)
      case Var(v) => Set(v)
      case KTuple(elems) => elems.toSet
      case Array(x, y) => Set(x, y)
      case Get(x, y) => Set(x, y)
      case Put(x, y, z) => Set(x, y, z)
      case Apply(fn, args) => Set(fn) &~ directFunctions &~ localFns | args.toSet
      case ApplyExternal(_, args) => args.toSet
      case IfCmp(_, x, y, tru, fls) =>
        Set(x, y) | indirectFVs(tru, localFns) | indirectFVs(fls, localFns)
      case Let(entry, bound, kont) =>
        indirectFVs(kont, localFns) - entry.name | indirectFVs(bound, localFns)
      case LetTuple(elems, bound, kont) =>
        indirectFVs(kont, localFns) &~ elems.map(_.name).toSet | Set(bound)
      case LetRec(FDef(Entry(name, _), args, body, _), kont) =>
        val lf = localFns + name
        indirectFVs(body, lf) &~ args.map(_.name).toSet | indirectFVs(kont, lf)
    }
  }

  private[this] def closure(norm: KNorm, scope: Scope): KClosure =
    norm.raw match {
      case raw: KCRaw => KClosure(norm.comment, raw)
      case Apply(fn, args) =>
        if (directFunctions contains fn) {
          KClosure(norm.comment, ApplyDirect(LabelID(fn.name), args))
        } else {
          KClosure(norm.comment, ApplyClosure(fn, args))
        }
      case ApplyExternal(fn, args) =>
        KClosure(norm.comment, ApplyDirect(LabelID(s"$$ext_${fn.name}"), args))
      case IfCmp(op, left, right, tru, fls) =>
        KClosure(norm.comment, CIfCmp(op, left, right, closure(tru, Local), closure(fls, Local)))
      case Let(entry, bound, kont) =>
        if (scope == Global && !bound.raw.mayHaveEffect) {
          println(s"[Closurify] no mutate: ${entry.name}")
          globalConstsRev ::= (entry, closure(bound, Local))
          globalConstNames += entry.name
          closure(kont, Global)
        } else {
          if (scope == Global) println(s"[Closurify] may mutate: ${entry.name}")
          KClosure(norm.comment, CLet(entry, closure(bound, Local), closure(kont, Local)))
        }
      case LetTuple(elems, bound, kont) =>
        KClosure(norm.comment, CLetTuple(elems, bound, closure(kont, scope)))
      case LetRec(FDef(entry @ Entry(id @ ID(name), _), args, body, _), kont) =>
        val bodyFVs = (
          indirectFVs(body, Set(id)) -- args.map(_.name).toSet -- globalConstNames
          ).toList
        if (bodyFVs.isEmpty) {
          directFunctions += id
        }
        val kontFVs = indirectFVs(kont, Set()) -- globalConstNames
        val bodyKC = closure(body, Local)
        val kontKC = closure(kont, scope)
        globalFunctionsRev ::= CFDef(entry, args, bodyFVs, bodyKC)

        if (kontFVs contains id) {
          // kontでこの関数のクロージャを用いる
          println(s"[Closurify] closure for ${name} with fvs $bodyFVs")
          KClosure(norm.comment, CLetClosure(entry, LabelID(name), bodyFVs, kontKC))
        } else {
          println(s"[Closurify] no closure for ${name}")
          kontKC
        }
    }
}
