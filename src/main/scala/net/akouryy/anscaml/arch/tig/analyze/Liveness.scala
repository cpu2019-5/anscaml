package net.akouryy.anscaml.arch.tig
package analyze

import asm._

import scala.collection.mutable

object Liveness {
  type LiveSet = Set[XVar]

  private[this] type MutableInfo = mutable.Map[BlockIndex, List[LiveSet]]

  type Info = Map[BlockIndex, List[LiveSet]]

  private[this] def useInInst(inst: Instruction): Iterable[XVar] = inst match {
    case Mv(value) =>
      value.asXVar
    case _: Mvi | Nop | Read =>
      Set()
    case NewArray(len, elem) =>
      len.asVXVar ++ elem.asXVar
    case Store(addr, index, value) =>
      addr.asXVar ++ index.asVXVar ++ value.asXVar
    case Load(addr, index) =>
      addr.asXVar ++ index.asVXVar
    case UnOpTree(_, value) =>
      value.asXVar
    case BinOpVCTree(_, left, right) =>
      left.asXVar ++ right.asVXVar
    case BinOpVTree(_, left, right) =>
      left.asXVar ++ right.asXVar
    case Write(value) =>
      value.asXVar
    case CallDir(_, args) =>
      args.flatMap(_.asXVar)
    case _: Save | _: Restore => ???
  }

  private[this] def analyzeBlock(info: MutableInfo, c: Chart, b: Block): Unit = {
    var live: LiveSet = c.jumps(b.output) match {
      case _: StartFun => ???
      case Return(_, value, _) => value.asXVar.to(Set)
      case Condition(_, _, left, right, _, tru, fls) =>
        info(tru).head ++ info(fls).head ++ left.asXVar ++ right.asVXVar
      case Merge(_, inputs, outputID, output) =>
        info(output).head -- outputID.asXVar ++ inputs.find(_._2 == b.i).get._1.asXVar
    }
    val liveOut = live
    val liveInsRev = b.lines.reverseIterator.map { line =>
      live --= line.dest.asXVar
      live ++= useInInst(line.inst)
      live
    }.toList

    info(b.i) = (liveOut :: liveInsRev).reverse
  }

  def analyzeProgram(prog: Program): Info = {
    val info: MutableInfo = mutable.Map()

    for {
      f <- prog.functions
      a <- f.body.blocks.values.toSeq.reverseIterator
    } analyzeBlock(info, f.body, a)

    info.toMap
  }
}
