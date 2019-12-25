package net.akouryy.anscaml
package arch.tig
package analyze

import asm._
import base._

import scala.collection.mutable

object Liveness {
  type LiveSet = Set[XVar]

  private[this] type MutableInfo = mutable.Map[BlockIndex, List[LiveSet]]

  /**
    * 各Listは長さが対応するブロックの行数+1であり、先頭行のliveInから最終行のliveOutまでを持つ。
    */
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
    case Select(cond, tru, fls) =>
      cond.left.asXVar ++ cond.rightVC.asVXVar ++ tru.asXVar ++ fls.asXVar
    case Write(value) =>
      value.asXVar
    case CallDir(_, args, None) =>
      args.flatMap(_.asXVar)
    case _ => !!!!(inst)
  }

  private[this] def analyzeBlock(info: MutableInfo, c: Chart, b: Block): Unit = {
    var live: LiveSet = c.jumps(b.output) match {
      case _: StartFun => ???
      case Return(_, _, value, _) => value.asXVar.to(Set)
      case Branch(_, _, Branch.Cond(_, left, right), _, tru, fls) =>
        info(tru).head ++ info(fls).head ++ left.asXVar ++ right.asVXVar
      case Merge(_, _, inputs, outputID, output) =>
        info(output).head -- outputID.asXVar ++ inputs.find(_.bi == b.i).get.xid.asXVar
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
