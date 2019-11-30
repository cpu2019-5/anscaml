package net.akouryy.anscaml
package knorm

import base._

object Alpha {

  import KNorm._

  def apply(kn: KNorm): KNorm = convert(kn, Map())

  /**
    * @param env 旧IDから新IDへのマップ
    */
  private[this] def convert(kn: KNorm, env: Map[ID, ID]): KNorm = {
    @annotation.unused def apply(_k: KNorm): Nothing = ???

    def find(id: ID) = env.getOrElse(id, id)

    KNorm(
      kn.comment,
      kn.raw match {
        case KInt(_) | KFloat(_) | ExtArray(_) => kn.raw
        case BinOpTree(op, left, right) => BinOpTree(op, find(left), find(right))
        case IfCmp(op, left, right, tru, fls) =>
          IfCmp(op, find(left), find(right), convert(tru, env), convert(fls, env))
        case Var(v) => Var(find(v))
        case App(fn, args) => App(find(fn), args.map(find))
        case KTuple(elems) => KTuple(elems.map(find))
        case Get(array, index) => Get(find(array), find(index))
        case Put(array, index, value) => Put(find(array), find(index), find(value))
        case ExtFunApp(fn, args) => ExtFunApp(fn, args.map(find))
        case Let(Entry(v, typ), bound, kont) =>
          val v2 = ID.generate(v)
          Let(Entry(v2, typ), convert(bound, env), convert(kont, env + (v -> v2)))
        case LetTuple(elems, bound, kont) =>
          val elems2 = elems.map(e => (e.name, ID.generate(e.name), e.typ))
          LetTuple(
            elems2.map { case (_, e2, t) => Entry(e2, t) },
            find(bound),
            convert(kont, env ++ elems2.map { case (e, e2, _) => e -> e2 }),
          )
        case LetRec(FDef(Entry(fn, fnTyp), args, body, noInline), kont) =>
          val fn2 = ID.generate(fn)
          val args2 = args.map(a => (a.name, ID.generate(a.name), a.typ))
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
