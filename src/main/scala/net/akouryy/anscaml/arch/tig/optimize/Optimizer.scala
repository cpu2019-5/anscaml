package net.akouryy.anscaml
package arch.tig
package optimize

import asm.Program

object Optimizer {
  def apply(iterationCount: Int, asm: Program): Unit = {
    for (i <- 0 until iterationCount) {
      println(s"[TigOptimize] iteration $i")
      val changedER = EarlyReturn(asm)
      val changedMM = MergeMerge(asm)
      val changedIF = new ImmediateFolder(asm)()
      val (changedBT, useSets) = new BackwardTraverser()(asm)
      val changedDI = DistributeIf(asm, useSets)
      if (!changedER && !changedMM && !changedDI && !changedIF && !changedBT) return
    }
  }
}
