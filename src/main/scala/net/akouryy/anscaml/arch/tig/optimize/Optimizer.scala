package net.akouryy.anscaml
package arch.tig
package optimize

import asm.Program

object Optimizer {
  def apply(iterationCount: Int, asm: Program): Unit = {
    for (i <- 0 until iterationCount) {
      println(s"[TigOptimize] iteration $i")
      val changed =
        EarlyReturn(asm) | // 正格評価: 全最適化を実行
        MergeMerge(asm) |
        DistributeIf(asm) |
        new ImmediateFolder(asm)() |
        new BackwardTraverser()(asm)
      if (!changed) return
    }
  }
}
