package net.akouryy.anscaml
package arch.tig
package analyze

import asm._
import base._

import scala.collection.mutable

/**
  * Live variable analysis for SSA programs.
  *
  * <p>
  * To analyze for-loops correctly, it traverses the whole program twice.
  * <ul><li>
  * First, the live-in set of each ForLoopBottom jump is temporarily defined as: <br/>
  * `liveIn[bottom] = (liveIn[kont] ∪ liveCond[top]) \ (loop vars) ∪ (upd vars)`.
  * </li><li>
  * Second, it is calculated using the old live-in set of the loop body: <br/>
  * `liveIn[bottom] = (liveIn[kont] ∪ liveCond[top] ∪ liveIn[body]) \ (loop vars) ∪ (upd vars)`.
  * </li></ul>
  * It is sufficient to repeat this process just twice, since live-in set of the body does not
  * change in the second iteration.
  * </p>
  */
object Liveness {
  type LiveSet = Set[XVar]

  private[this] type MutableInfo = mutable.Map[BlockIndex, List[LiveSet]]

  /**
    * 各Listは長さが対応するブロックの行数+1であり、先頭行のliveInから最終行のliveOutまでを持つ。
    */
  type Info = Map[BlockIndex, List[LiveSet]]

  def useInInst(inst: Instruction): Iterable[XVar] = inst.usedXID.flatMap(_.asXVar)

  private[this] def analyzeBlock(info: MutableInfo, c: Chart, b: Block): Unit = {
    var live: LiveSet = c.jumps(b.output) match {
      case _: StartFun => !!!!(b)
      case Return(_, _, value, addr, _) => value.asXVar.toSet ++ addr.flatMap(_.asXVar)
      case Branch(_, _, Branch.Cond(_, left, right), _, tru, fls) =>
        info(tru).head ++ info(fls).head ++ left.asXVar ++ right.asVXVar
      case Merge(_, _, inputs, outputID, output) =>
        info(output).head -- outputID.asXVar ++ inputs.find(_.bi == b.i).get.xid.asXVar
      case ForLoopTop(_, _, cond, _, merges, _, _, body, kont) =>
        info(body).head ++ info(kont).head ++ cond.left.asXVar ++ cond.rightVC.asVXVar --
        merges.flatMap(_.loop.asXVar) ++ merges.flatMap(_.in.asXVar)
      case ForLoopBottom(_, _, _, loopTopIndex) =>
        /* (liveIn[kont] ∪ liveCond[top] ∪ liveIn[body]) \ (loop vars) ∪ (upd vars) */
        val top = c.jumps(loopTopIndex).asInstanceOf[ForLoopTop]
        info(top.kont).head ++ top.cond.left.asXVar ++ top.cond.rightVC.asVXVar ++
        info.get(top.body).foldF(_.head, Set()) -- top.merges.flatMap(_.loop.asXVar).toSet ++
        top.merges.flatMap(_.upd.asXVar).toSet
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
      _ <- Seq(0, 1) // see doc comment for object Liveness
      f <- prog.functions
      a <- f.body.blocks.values.toSeq.reverseIterator
    } analyzeBlock(info, f.body, a)

    info.toMap
  }

  def analyzeFunction(fun: FDef): Info = {
    /* gcSize and tyEnv is not used in analyzeProgram */
    analyzeProgram(Program(0, Map(), List(fun)))
  }
}
