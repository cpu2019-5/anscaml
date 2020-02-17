package net.akouryy.anscaml
package knorm
package optimize

import base._
import knorm.KNorm._
import syntax.CmpOp

import scala.annotation.tailrec

/**
  * 停止条件が引数と不変変数の比較であるような再帰関数を置き換えてループにする。
  * ループ前後で不変な(`a_i = b_i`なる)変数は予め除いておく(`a_i`を使い回す)。
  * {{{let rec f a... =
  *   X;                                         (* defines p... (outerVars) *)
  *   if ap_i <=> c then (
  *     Y;                                       (* may define b... *)
  *     f b...
  *   ) else
  *     Z}}}
  * ↓
  * {{{let f a... =
  *   X;                                         (* defines p... *)
  *   FOR l...=ap...; l_i <=> c; l...=bq... DO
  *     [l/ap]Y;                                 (* may define b... *)
  *     [b/a][[q/p]]X;                           (* defines q... *)
  *   END;
  *   [l/ap]Z}}}
  *
  * ちょっとだけ参考: https://eguchishi.hatenablog.com/entry/2017/09/09/150229
  */
class LoopDetector {
  def apply(kn: KNorm): KNorm = {
    detectGeneralLoop(detectForLoop(kn))
  }

  /**
    * @return Option of (generator of the new body from a loop expression;
    *         the loop condition; the loop body; the expression following the loop)
    */
  private[this] def digExprX(kn: KNorm): Option[(KNorm => KNorm, (CmpOp, ID, ID), KNorm, KNorm)] = {
    kn.raw match {
      case IfCmp(op, left, right, tru, fls) => Some((identity, (op, left, right), tru, fls))
      case raw: HasKont =>
        for ((x, c, t, f) <- digExprX(raw.kont)) yield
          ((w: KNorm) => kn.copy(raw = raw.copyWithKont(x(w))), c, t, f)
      case _ => None
    }
  }

  private[this] def digExprY(kn: KNorm, fn: ID): Option[(KNorm => KNorm, Apply)] = {
    kn.raw match {
      case raw @ Apply(`fn`, _, _) => Some(identity, raw)
      case raw: HasKont =>
        for ((y, c) <- digExprY(raw.kont, fn)) yield
          ((w: KNorm) => kn.copy(raw = raw.copyWithKont(y(w))), c)
      // case _: IfCmp => None
      case _ => None
    }
  }

  /**
    * @return (Set of fixed vars; Set of unfixed vars)
    */
  @tailrec private[this] def findFixedAndUnfixedOuterVars(kn: KNorm, accF: Set[ID], accU: Set[ID])
  : (Set[ID], Set[ID]) = {
    object Fixed {
      def unapply(x: ID): Option[ID] = Option.when(accF contains x)(x)
    }

    kn.raw match {
      case Let(Entry(name, _), KNorm(_,
      KInt(_) | KFloat(_) | BinOpTree(_, Fixed(_), Fixed(_)) | Var(Fixed(_)) | KTuple(Nil)
      // TODO: KTuple
      ), kont) =>
        findFixedAndUnfixedOuterVars(kont, accF + name, accU)
      case Let(Entry(name, _), _, kont) =>
        findFixedAndUnfixedOuterVars(kont, accF, accU + name)
      case LetTuple(elems, Fixed(_), kont) =>
        findFixedAndUnfixedOuterVars(kont, accF ++ elems.map(_.name), accU)
      case LetTuple(elems, _, kont) =>
        findFixedAndUnfixedOuterVars(kont, accF, accU ++ elems.map(_.name))
      case raw: HasKont =>
        findFixedAndUnfixedOuterVars(raw.kont, accF, accU)
      case _ => (accF, accU)
    }
  }

  private[this] def detectFLInFun(fDef: FDef, negated: Boolean): Option[KNorm] = {
    val fnName = fDef.entry.name
    for {
      (xGen, (op, left, right), tru, fls) <- digExprX(fDef.body)
      (recBody, z) = if (negated) (fls, tru) else (tru, fls)
      (yGen, recCall) <- digExprY(recBody, fnName)
      args = fDef.args.map(_.name)
      fixedArgs = args.zipStrict(recCall.args)
        .filter { case (p, a) => p == a }.map(_._2).toSet
      (fixedOuterVars, unfixedOuterVars)
      = findFixedAndUnfixedOuterVars(xGen(KNorm(NC, KTuple(Nil))), fixedArgs, Set())
      isRightFixed = (args ++ unfixedOuterVars).contains(left) && fixedOuterVars.contains(right)
      () = println(s"[KO-LD] ${fnName.str}: ${left.str} $op ${right.str}, " +
                   s"fixed args ${fixedArgs.map(_.str)}, " +
                   s"fixed vars ${fixedOuterVars.map(_.str)}")
      if isRightFixed || fixedOuterVars.contains(left) && args.contains(right)
      initVars = args.filter(!fixedArgs.contains(_)) ++ unfixedOuterVars
      loopVars = initVars.map(a => ID.generate(a.str + ID.Special.KO_LOOP_VAR))
      updateOuterVars = unfixedOuterVars.map(ov => ID.generate(ov.str + ID.Special.KO_UPD_VAR))
      updateVars = recCall.args.filter(!fixedArgs.contains(_)) ++ updateOuterVars
      i2l = initVars.zipStrict(loopVars).toMap
    } yield {
      println(s"[KO-LD] ${fnName.str}: ${left.str} $op ${right.str}, " +
              s"fixed args ${fixedArgs.map(_.str)}, " +
              s"fixed vars ${fixedOuterVars.map(_.str)}")

      xGen(KNorm(CM(s"[KO-LD] start loop for ${fnName.str}"),
        ForCmp(
          op, i2l.getOrElse(left, left), i2l.getOrElse(right, right), negated, loopVars, initVars,
          yGen(
            xGen(
              KNorm(CM("[KO-LD] update vars"), LoopUpdater(updateVars))
            ).replaceVars(initVars.zipStrict(updateVars).toMap)
          ).replaceVars(i2l),
          z.replaceVars(i2l),
        )
      ))
    }
  }

  private[this] def detectGLInFun(fDef: FDef): Option[KNorm] = {
    var found = false

    def glRec(kn: KNorm): KNorm = kn.copy(raw = kn.raw match {
      case _: KInt | _: KFloat | _: BinOpTree | _: Var | _: KTuple | _: LoopUpdater | _: KArray |
           _: Get | _: Put | _: ApplyExternal => kn.raw
      case IfCmp(op, left, right, tru, fls) =>
        IfCmp(op, left, right, glRec(tru), glRec(fls))
      case kr: HasKont => kr.copyWithKont(glRec(kr.kont)) /* convert tail only */
      case a @ Apply(fn, args, _) =>
        if (fn == fDef.entry.name) {
          found = true
          LoopUpdater(args)
        } else {
          a
        }
      case _: GeneralLoop => !!!!(kn)
    })

    val result = glRec(fDef.body)
    Option.when(found) {
      val loopVars = fDef.args.map(arg =>
        arg.name -> ID.generate(arg.name.str + ID.Special.KO_GENERAL_LOOP_VAR)
      )
      KNorm(
        NC,
        GeneralLoop(loopVars.map(_._2), loopVars.map(_._1), result.replaceVars(loopVars.toMap)),
      )
    }
  }

  private[this] def detectLoop(inFun: FDef => Option[KNorm])(kn: KNorm): KNorm = {
    kn.copy(raw = kn.raw match {
      case raw: IfCmp =>
        raw.copy(tru = detectLoop(inFun)(raw.tru), fls = detectLoop(inFun)(raw.fls))
      case raw: ForCmp =>
        raw.copy(body = detectLoop(inFun)(raw.body), kont = detectLoop(inFun)(raw.kont))
      case raw: Let =>
        raw.copy(bound = detectLoop(inFun)(raw.bound), kont = detectLoop(inFun)(raw.kont))
      case raw: LetTuple =>
        raw.copy(kont = detectLoop(inFun)(raw.kont))
      case LetRec(fDef, kont) =>
        inFun(fDef) match {
          case Some(body) => LetRec(fDef.copy(body = body), detectLoop(inFun)(kont))
          case None => LetRec(fDef, detectLoop(inFun)(kont))
        }
      case raw => raw
    })
  }

  private[this] def detectForLoop =
    detectLoop(f => detectFLInFun(f, negated = false).orElse(detectFLInFun(f, negated = true))) _

  private[this] def detectGeneralLoop = detectLoop(detectGLInFun) _
}
