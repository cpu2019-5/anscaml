package net.akouryy.anscaml
package knorm
package optimize

import KNorm._

/**
  * let平坦化
  * 単体では特に最適化効果はないが、多分ほかの最適化がやりやすくなる？
  * 少なくとも出力は人間に優しくなる
  */
object Assoc {
  def apply(kn: KNorm): KNorm = {
    Logger.log("KO-AS", "Start")
    flatten(kn)
  }

  private[this] def flatten(kn0: KNorm): KNorm = {
    kn0.raw match {
      case IfCmp(op, left, right, tru, fls) =>
        kn0.copy(raw = IfCmp(op, left, right, flatten(tru), flatten(fls)))
      case raw: ForCmp =>
        kn0.copy(raw = raw.mapBodyKont(flatten)(flatten))
      case LetTuple(elems, bound, kont) =>
        kn0.copy(raw = LetTuple(elems, bound, flatten(kont)))
      case LetRec(fDef, kont) =>
        kn0.copy(raw = LetRec(fDef.copy(body = flatten(fDef.body)), flatten(kont)))
      case Let(entry0, bound0, kont0) =>
        def insert(kn1: KNorm): KNorm = kn1.raw match {
          case Let(entry1, bound1, kont1) =>
            kn1.copy(raw = Let(entry1, bound1, insert(kont1)))
          case LetTuple(elems1, bound1, kont1) =>
            kn1.copy(raw = LetTuple(elems1, bound1, insert(kont1)))
          case LetRec(fDef1, kont1) =>
            kn1.copy(raw = LetRec(fDef1, insert(kont1)))
          case _ =>
            kn0.copy(raw = Let(entry0, kn1, flatten(kont0)))
        }

        insert(flatten(bound0))
      case _ => kn0
    }
  }

}
