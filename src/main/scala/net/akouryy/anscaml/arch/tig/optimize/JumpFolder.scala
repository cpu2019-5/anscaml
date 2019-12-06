package net.akouryy.anscaml
package arch.tig
package optimize

import asm._
import base._

/**
  * ==単独併合除去==
  * Mergeジャンプの入力が1つしかない場合上下のブロックを繋いで1つのブロックにする。
  *
  * ==定数分岐除去==
  * 定数標準形(%r0 Eq C(0) または %r0 Eq C(1))のConditionジャンプを削除し単独Mergeにする。
  */
class JumpFolder {

  private[this] def removeBlock(chart: Chart, bi0: BlockIndex): Unit = {
    val ji1 = chart.blocks.remove(bi0).get.output
    chart.jumps(ji1) match {
      case _: StartFun => ???
      case _: Return => chart.jumps -= ji1
      case Condition(_, _, _, _, _, tru2, fls2) =>
        chart.jumps -= ji1
        removeBlock(chart, tru2)
        removeBlock(chart, fls2)
      case m @ Merge(_, inputs, _, bi2) =>
        if (inputs.sizeIs == 1) {
          chart.jumps -= ji1
          removeBlock(chart, bi2)
        } else {
          chart.jumps(ji1) = m.copy(inputs = inputs.filter(_._2 != bi0))
        }
    }
  }

  private[this] def concatBlock(
    c: Chart, bi0: BlockIndex, bi2: BlockIndex
  ): Unit = {
    val b0 = c.blocks(bi0)
    val b2 = c.blocks(bi2)
    c.blocks(bi0) = b0.copy(lines = b0.lines ::: b2.lines, output = b2.output)
    c.blocks -= bi2
    c.jumps(b2.output) = c.jumps(b2.output) match {
      case _: StartFun => ???
      case j: Return => j.copy(input = bi0)
      case j: Condition => j.copy(input = bi0)
      case j: Merge => j.copy(inputs =
        j.inputs.map { case xid -> bi => xid -> (if (bi == bi2) bi0 else bi) }
      )
    }
  }

  def apply(program: Program): Boolean = {
    var changed = false

    for (f <- program.functions) {
      /*f.body.jumps.valuesIterator.foreach*/
      for (k <- f.body.jumps.keysIterator; j <- f.body.jumps.get(k)) j match {
        case Condition(ji1, Eq, XReg.REG_ZERO, C(Word(i @ (0 | 1), _)), bi0, tbi2, fbi2) =>
          changed = true
          val toUse = if (i == 0) tbi2 else fbi2
          val toRemove = if (i == 0) fbi2 else tbi2

          f.body.jumps -= ji1
          concatBlock(f.body, bi0, toUse)

          removeBlock(f.body, toRemove)

        case Merge(ji1, List(id0 -> bi0), id2, bi2) if id0 == id2 || id2 == XReg.REG_DUMMY =>
          f.body.jumps -= ji1
          concatBlock(f.body, bi0, bi2)

        case _ =>
      }
    }

    changed
  }
}
