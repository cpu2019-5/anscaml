package net.akouryy.anscaml
package arch.tig

import algo.Graph
import asm._

import scala.collection.mutable

class Scheduler(prog: Program) {
  private[this] def dependentParentGraph(lines: List[Line]): Graph = {
    val destss = Array.fill(lines.length)(mutable.Set[Int]())
    for {
      Seq((Line(_, liDef, liInst), i), (Line(_, ljDef, ljInst), j))
        <- lines.zipWithIndex.combinations(2)
      if liInst.usedXID.contains(ljDef) /* Register WAR */ ||
         ljInst.usedXID.contains(liDef) /* Register RAW */ ||
         liDef == ljDef /* Register WAW */ ||
         ((liInst, ljInst) match {
           case (Read, Read) /* IO RAR */ |
                (_: Write, _: Write) /* IO WAW */ |
                (_: Store, _: Load) /* Memory WAR */ |
                (_: Load, _: Store) /* Memory RAW */ |
                (_: Store, _: Store) /* Memory WAW */ |
                (_: NewArray, _) | (_, _: NewArray) /* Memory ?A? */ |
                (_: CallDir, _) | (_, _: CallDir) /* function ?A? */
           => true
           case _ => false
         })
    } {
      destss(j) += i
    }
    Graph(destss.map(_.toSet))
  }

  private[this] def calcPriority(lines: List[Line], parents: Graph, children: Graph)
  : IndexedSeq[Int] = {
    val ret = Array.fill(lines.length)(0)
    val que = mutable.Queue[Int]()
    for {
      i <- lines.indices
      if children(i).isEmpty
    } {
      ret(i) = lines(i) match {
        case Line(_, _, _: Load) => -2
        case _ => -1
      }
      que.enqueueAll(parents(i))
    }
    while (que.nonEmpty) {
      val i = que.dequeue()
      if (ret(i) == 0) {
        val base = children(i).map(ret).min
        if (base < 0) {
          ret(i) = base + (lines(i) match {
            case Line(_, _, _: Load) => -2
            case _ => -1
          })
          que.enqueueAll(parents(i))
        }
      }
    }
    ret.toIndexedSeq
  }

  private[this] def scheduleLine(block: Block): List[Line] = {
    val parents = dependentParentGraph(block.lines)
    val children = parents.reversed
    val used = mutable.Set[Int]()
    val waiting = mutable.Set[Int]()
    val ret = mutable.ListBuffer[Line]()

    val priority = calcPriority(block.lines, parents, children)
    val available = mutable.PriorityQueue[Int]()(
      Ordering.by[Int, (Int, Int)](i => (priority(i), -i))
    )
    for {
      i <- block.lines.indices
      if parents(i).isEmpty
    } available.enqueue(i)

    while (available.nonEmpty || waiting.nonEmpty) {
      if (available.isEmpty) {
        available ++= waiting
        waiting.clear()
      }
      val i = available.dequeue()
      ret += block.lines(i)
      used += i

      available ++= waiting
      waiting.clear()
      for {
        j <- children(i)
        if parents(j).forall(used)
      } block.lines(i) match {
        case Line(_, _, _: Load) => waiting += j
        case _ => available += j
      }
    }

    assert(ret.sizeIs == block.lines.length)
    ret.toList
  }

  def schedule(): Unit = {
    for (fn <- prog.functions; block <- fn.body.blocks.values.toSeq.reverse) {
      fn.body.blocks(block.i) = block.copy(lines = scheduleLine(block))
    }
  }

}
