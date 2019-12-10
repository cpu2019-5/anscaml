package net.akouryy.anscaml
package arch.tig
package optimize

import asm._

object EarlyReturn {
  def apply(program: Program): Boolean = {
    var changed = false
    for (f <- program.functions) {
      val c = goBackFrom(f.body.jumps.lastKey, f.body)
      changed ||= c
    }
    changed
  }

  private[this] def goBackFrom(ji0: JumpIndex, chart: Chart): Boolean = {
    chart.jumps(ji0) match {
      case Return(cm0, _, retID, bi1) =>
        chart.blocks(bi1) match {
          case Block(_, Nil, ji2, _) =>
            chart.jumps(ji2) match {
              case Merge(_, _, inputs, outputID, _) if retID == outputID =>
                chart.jumps.remove(ji0)
                chart.blocks.remove(bi1)
                chart.jumps.remove(ji2)
                for ((mergingID, bi3) <- inputs) {
                  val ji4 = JumpIndex.generate()
                  chart.jumps(ji4) = Return(cm0, ji4, mergingID, bi3)
                  chart.blocks(bi3) = chart.blocks(bi3).copy(output = ji4)
                  goBackFrom(ji4, chart)
                }
                true
              case _ => false
            }
          case _ => false
        }
      case _ => ???
    }
  }
}
