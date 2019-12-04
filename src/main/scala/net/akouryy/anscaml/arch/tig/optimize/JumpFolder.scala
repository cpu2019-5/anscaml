package net.akouryy.anscaml
package arch.tig
package optimize

import asm._

/**
  * ==単独併合除去 (not implemented)==
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

  def apply(program: Program): Boolean = {
    var changed = false

    for (f <- program.functions) {
      for {
        Condition(ji1, syntax.CmpOp.Eq, AReg.REG_ZERO, C(i @ (0 | 1)), bi0, tbi2, fbi2)
          <- f.body.jumps.valuesIterator
      } {
        changed = true
        val toUse = if (i == 0) tbi2 else fbi2
        val toRemove = if (i == 0) fbi2 else tbi2

        f.body.jumps(ji1) = Merge(ji1, List(AReg.REG_DUMMY -> bi0), AReg.REG_DUMMY, toUse)

        removeBlock(f.body, toRemove)
      }
    }

    changed
  }
}
