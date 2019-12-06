package net.akouryy.anscaml.arch.tig
package analyze

import asm._

import scala.collection.mutable

object Liveness {
  type LiveSet = Set[AVar]

  private[this] type MutableInfo = mutable.Map[BlockIndex, List[LiveSet]]

  type Info = Map[BlockIndex, List[LiveSet]]

  private[this] def useInInst(inst: Instruction): Iterable[AVar] = inst match {
    case Mv(value) =>
      value.aVarOpt
    case _: Mvi | _: Fmvi | Nop | Read =>
      Set()
    case NewArray(len, elem) =>
      len.vAVarOpt ++ elem.aVarOpt
    case Store(addr, index, value) =>
      addr.aVarOpt ++ index.vAVarOpt ++ value.aVarOpt
    case Load(addr, index) =>
      addr.aVarOpt ++ index.vAVarOpt
    case UnOpTree(_, value) =>
      value.aVarOpt
    case BinOpVCTree(_, left, right) =>
      left.aVarOpt ++ right.vAVarOpt
    case BinOpVTree(_, left, right) =>
      left.aVarOpt ++ right.aVarOpt
    case Write(value) =>
      value.aVarOpt
    case CallDir(_, args) =>
      args.flatMap(_.aVarOpt)
    case _: Save | _: Restore => ???
  }

  private[this] def analyzeBlock(info: MutableInfo, c: Chart, b: Block): Unit = {
    var live: LiveSet = c.jumps(b.output) match {
      case _: StartFun => ???
      case Return(_, value, _) => value.aVarOpt.to(Set)
      case Condition(_, _, left, right, _, tru, fls) =>
        info(tru).head ++ info(fls).head ++ left.aVarOpt ++ right.vAVarOpt
      case Merge(_, inputs, outputID, output) =>
        info(output).head -- outputID.aVarOpt ++ inputs.find(_._2 == b.i).get._1.aVarOpt
    }
    val liveOut = live
    val liveInsRev = b.lines.reverseIterator.map { line =>
      live --= line.dest.aVarOpt
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
