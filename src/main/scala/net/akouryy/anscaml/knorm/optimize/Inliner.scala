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
      case ForCmp(_, _, _, _, _, _, body, kont) => 1 + size(body) + size(kont)
      case Let(_, bound, kont) => 1 + size(bound) + size(kont)
      case LetTuple(_, _, kont) => 1 + size(kont)
      case LetRec(FDef(_, _, body, _), kont) => 1 + size(body) + size(kont)
      case _ => 1
    }
  }
}

class Inliner {

  import Inliner._

  private[this] val bodyEnv = mutable.Map[ID, (Int, FDef)]()
  private[this] var optimizerIterationCount = 0

  def apply(kn: KNorm, optimizerIterationCount: Int): KNorm = {
    Logger.log("KO-IL", "Start")
    bodyEnv.clear()
    this.optimizerIterationCount = optimizerIterationCount
    embed(ID("AnsMain"), kn)
  }

  private[this] def embed(scopeFn: ID, kn: KNorm): KNorm = kn.raw match {
    case IfCmp(op, left, right, tru, fls) =>
      kn.copy(raw = IfCmp(op, left, right, embed(scopeFn, tru), embed(scopeFn, fls)))
    case raw: ForCmp =>
      kn.copy(raw = raw.mapBodyKont(embed(scopeFn, _))(embed(scopeFn, _)))
    case Let(entry, bound, kont) =>
      kn.copy(raw = Let(entry, embed(scopeFn, bound), embed(scopeFn, kont)))
    case LetTuple(elems, bound, kont) =>
      kn.copy(raw = LetTuple(elems, bound, embed(scopeFn, kont)))
    case LetRec(fDef @ FDef(Entry(name, _), _, body, annot), kont) =>
      if (!annot.contains(Annot.NoInline) && size(body) <= AnsCaml.config.inlineLimit) {
        bodyEnv(name) = (size(body), fDef)
      }
      val newBody = embed(name, body)
      val newFDef = fDef.copy(body = newBody)
      /*if (!annot.contains(Annot.NoInline) && (
        size(body) > AnsCaml.config.inlineLimit / 5 && size(newBody) <= AnsCaml.config.inlineLimit
        || size(newBody) <= AnsCaml.config.inlineLimit / 5)) {
        bodyEnv(name) = (size(newBody), newFDef)
      }*/
      kn.copy(raw = LetRec(newFDef, embed(scopeFn, kont)))

    case Apply(fn, args, isRecCall) =>
      bodyEnv.get(fn) match {
        case Some((size, fDef)) if size <= AnsCaml.config.inlineLimit / (
          if (isRecCall) 5 * (optimizerIterationCount + 1) else 1
          ) =>
          val body = Alpha.convert(fDef.body, fDef.args.map(_.name).zip(args).toMap)
          KNorm(
            kn.comment + body.comment :+ s"[KO Inliner] ${fn.str} in ${scopeFn.str}",
            body.raw
          )
        case _ => kn
      }
    case _ => kn
  }
}
