package net.akouryy.anscaml
package knorm
package optimize

import base._
import syntax.Annot
import KNorm._

import scala.collection.mutable

object Inliner {
  def size(kn: KNorm): Int = {
    import KNorm._
    kn.raw match {
      case IfCmp(_, _, _, tru, fls) => 1 + size(tru) + size(fls)
      case Let(_, bound, kont) => 1 + size(bound) + size(kont)
      case LetTuple(_, _, kont) => 1 + size(kont)
      case LetRec(FDef(_, _, body, _), kont) => 1 + size(body) + size(kont)
      case _ => 1
    }
  }
}

class Inliner {

  import Inliner._

  private[this] val bodyEnv = mutable.Map[ID, FDef]()

  def apply(kn: KNorm): KNorm = {
    println("[KOptimize Inliner] Start")
    bodyEnv.clear()
    embed(ID("AnsMain"), kn)
  }

  private[this] def embed(scopeFn: ID, kn: KNorm): KNorm = kn.raw match {
    case IfCmp(op, left, right, tru, fls) =>
      kn.copy(raw = IfCmp(op, left, right, embed(scopeFn, tru), embed(scopeFn, fls)))
    case Let(entry, bound, kont) =>
      kn.copy(raw = Let(entry, embed(scopeFn, bound), embed(scopeFn, kont)))
    case LetTuple(elems, bound, kont) =>
      kn.copy(raw = LetTuple(elems, bound, embed(scopeFn, kont)))
    case LetRec(fDef @ FDef(Entry(name, _), _, body, annot), kont) =>
      if (!annot.contains(Annot.NoInline) && size(body) <= AnsCaml.config.inlineLimit) {
        // TODO: sizeIs
        bodyEnv(name) = fDef
      }
      kn.copy(raw = LetRec(fDef.copy(body = embed(name, body)), embed(scopeFn, kont)))

    case Apply(fn, args) if bodyEnv contains fn =>
      // println(s"[KOptimize Inliner] ${fn.name} in ${scopeFn.name}")
      val fDef = bodyEnv(fn)
      val body = Alpha.convert(fDef.body, fDef.args.map(_.name).zip(args).toMap)
      KNorm(
        kn.comment + body.comment :+ s"[KO Inliner] ${fn.name} in ${scopeFn.name}",
        body.raw
      )
    case _ => kn
  }
}
