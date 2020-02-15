package net.akouryy.anscaml
package knorm
package analyze

import base._
import KNorm._
import swarm.SwarmIndex

import scala.collection.mutable

class LoadTimeConstantDetector(sw: Map[ID, swarm.SwarmIndex]) {
  private[this] val results = mutable.Map[ID, LTCDResult]()
  var x = false

  /**
    * Calling this method twice makes the result correct.
    */
  private[this] def traverse(kc: KClosed): LTCDResult = {
    kc.raw match {
      case _: KInt | _: KFloat | _: BinOpTree | _: Var | _: KTuple | _: ForUpdater | _: KArray =>
        new LTCDResult()
      case Get(array, _) => new LTCDResult(sw(array) -> GetOnly)
      case Put(array, _, _) => new LTCDResult(sw(array) -> PutOnly)
      case ApplyDirect(fn, _) => results.getOrElse(ID(fn), new LTCDResult())
      case CIfCmp(_, _, _, tru, fls) => traverse(tru) | traverse(fls)
      case CForCmp(_, _, _, _, _, _, body, kont) =>
        val b = traverse(body)
        b ~ b ~ traverse(kont)
      case CLet(_, bound, kont) => traverse(bound) ~ traverse(kont)
      case CLetTuple(_, _, kont) => traverse(kont)
      case _ => !!!!(kc)
    }
  }

  def detect(prog: KNorm.KCProgram): Set[SwarmIndex] = {
    results.clear()
    for (_ <- 0 to 1; fn <- prog.fDefs) {
      results(fn.entry.name) = traverse(fn.body)
      println(s"${fn.entry.name} si16: ${results(fn.entry.name).kinds.get(SwarmIndex(16))}")
    }
    x = true
    traverse(prog.main).kinds.filter(_._2 != Mixed).keySet
  }

  private[this] sealed trait LTCDKind {
    /**
      * Parallel composition
      */
    def |(that: LTCDKind): LTCDKind = (this, that) match {
      case (Unused, _) => that
      case (_, Unused) => this
      case (Mixed, _) | (_, Mixed) => Mixed
      case _ if this == that => this
      case _ => PutThenGet
    }

    /**
      * Sequential composition
      */
    def ~(that: LTCDKind): LTCDKind = (this, that) match {
      case (GetOnly | PutThenGet, PutOnly | PutThenGet) => Mixed
      case _ => this | that
    }
  }

  private[this] case object Unused extends LTCDKind

  private[this] case object PutOnly extends LTCDKind

  private[this] case object GetOnly extends LTCDKind

  private[this] case object PutThenGet extends LTCDKind

  private[this] case object Mixed extends LTCDKind

  private[this] final class LTCDResult private(val kinds: Map[SwarmIndex, LTCDKind]) {
    def this(kinds: (SwarmIndex, LTCDKind)*) = this(Map(kinds: _*))

    def |(that: LTCDResult): LTCDResult = {
      new LTCDResult((kinds.keySet ++ that.kinds.keys).map(
        id => id -> (kinds.getOrElse(id, Unused) | that.kinds.getOrElse(id, Unused))
      ).toMap)
    }

    def ~(that: LTCDResult): LTCDResult = {
      new LTCDResult((kinds.keySet ++ that.kinds.keys).map(
        id => id -> (kinds.getOrElse(id, Unused) ~ that.kinds.getOrElse(id, Unused))
      ).toMap)
    }
  }

}
