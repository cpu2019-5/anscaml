package net.akouryy.anscaml
package arch.tig
package optimize

import asm.Program

object Optimizer {
  def apply(iterationCount: Int, asm: Program): Unit = {
    for (i <- 0 until iterationCount) {
      println(s"[TigOptimize] iteration $i")
      val changedER = false // EarlyReturn(asm)
      val changedMM = false // MergeMerge(asm)
      val changedIF = false // new ImmediateFolder(asm)()
      val changedJF = false // new JumpFolder()(asm)
      val (changedBT, useSets) = new BackwardTraverser()(asm)
      val changedDI = false // DistributeIf(asm, useSets)
      if (!changedER && !changedMM && !changedDI && !changedIF && !changedBT && !changedJF) return
    }
  }
}
