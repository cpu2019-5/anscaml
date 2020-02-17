package net.akouryy.anscaml
package knorm

import base._

object Alpha {

  import KNorm._

  def apply(kn: KNorm): KNorm = convert(kn, Map())

  /**
    * @note Inliner からアクセスするため private[knorm]
    * @param env 旧IDから新IDへのマップ
    */
  private[knorm] def convert(kn: KNorm, env: Map[ID, ID]): KNorm = {
    @annotation.unused def apply(_k: KNorm): Nothing = ???

    def find(id: ID) = env.getOrElse(id, id)

    KNorm(
      kn.comment,
      kn.raw match {
        case KInt(_) | KFloat(_) => kn.raw
        case BinOpTree(op, left, right) => BinOpTree(op, find(left), find(right))
        case Var(v) => Var(find(v))
        case Apply(fn, args, isRecCall) => Apply(find(fn), args.map(find), isRecCall)
        case KTuple(elems) => KTuple(elems.map(find))
        case LoopUpdater(elems) => LoopUpdater(elems.map(find))
        case KArray(len, elem) => KArray(find(len), find(elem))
        case Get(array, index) => Get(find(array), find(index))
        case Put(array, index, value) => Put(find(array), find(index), find(value))
        case ApplyExternal(fn, args) => ApplyExternal(fn, args.map(find))
        case IfCmp(op, left, right, tru, fls) =>
          IfCmp(op, find(left), find(right), convert(tru, env), convert(fls, env))
        case ForCmp(op, left, right, negated, loopVars, initVars, body, kont) =>
          val newLVs = loopVars.map(lv => lv -> ID.generate(lv.str))
          val newEnv = env ++ newLVs
          ForCmp(
            op, newEnv.getOrElse(left, left), newEnv.getOrElse(right, right),
            negated, newLVs.map(_._2), initVars.map(find),
            convert(body, newEnv), convert(kont, newEnv),
          )
        case GeneralLoop(loopVars, initVars, body) =>
          val newLVs = loopVars.map(lv => lv -> ID.generate(lv.str))
          val newEnv = env ++ newLVs
          GeneralLoop(newLVs.map(_._2), initVars.map(find), convert(body, newEnv))
        case Let(Entry(v, typ), bound, kont) =>
          val v2 = ID.generate(v.str)
          Let(Entry(v2, typ), convert(bound, env), convert(kont, env + (v -> v2)))
        case LetTuple(elems, bound, kont) =>
          val elems2 = elems.map(e => (e.name, ID.generate(e.name.str), e.typ))
          LetTuple(
            elems2.map { case (_, e2, t) => Entry(e2, t) },
            find(bound),
            convert(kont, env ++ elems2.map { case (e, e2, _) => e -> e2 }),
          )
        case LetRec(FDef(Entry(fn, fnTyp), args, body, noInline), kont) =>
          val fn2 = ID.generate(fn.str)
          val args2 = args.map(a => (a.name, ID.generate(a.name.str), a.typ))
          val envFn = env + (fn -> fn2)
          LetRec(
            FDef(
              Entry(fn2, fnTyp),
              args2.map { case (_, a2, t) => Entry(a2, t) },
              convert(body, envFn ++ args2.map { case (a, a2, _) => a -> a2 }),
              noInline
            ),
            convert(kont, envFn)
          )
      }
    )
  }
}
