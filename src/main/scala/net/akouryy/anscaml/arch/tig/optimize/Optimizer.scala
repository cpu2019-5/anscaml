package net.akouryy.anscaml
package arch.tig
package optimize

import asm.Program

object Optimizer {
  def apply(iterationCount: Int, asm: Program): Unit = {
    for (i <- 0 until iterationCount) {
      println(s"[TigOptimize] iteration $i")
      val changedER = EarlyReturn(asm)
      if (!changedER) return
    }
  }
}
