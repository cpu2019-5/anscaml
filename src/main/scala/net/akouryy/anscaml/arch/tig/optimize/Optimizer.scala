package net.akouryy.anscaml
package arch.tig
package optimize

import asm.Program

import scala.util.Using

object Optimizer {
  def apply(iterationCount: Int, asm: Program): Unit = {
    for (i <- 0 until iterationCount) {
      Logger.log("TO", s"iteration $i")
      var changed = false
      changed = EarlyReturn(asm) || changed // avoid short-circuit evaluation
      changed = MergeMerge(asm) || changed
      val (changedBT2, _) = new BackwardTraverser()(asm)
      changed = changedBT2 || changed
      changed = new ImmediateFolder(asm)() || changed
      changed = new JumpFolder()(asm) || changed
      val (changedBT, useSets) = new BackwardTraverser()(asm)
      changed = changedBT || changed
      changed = DistributeIf(asm, useSets) || changed
      changed = ComplexFolder(asm) || changed
      AliasSolver(asm)
      if (!changed) return
      /*if (AnsCaml.config.xGenerateAsmGraphs) {
        Using.resource(new java.io.PrintWriter(s"../temp/to-$i.dot")) {
          _.write(new arch.tig.GraphDrawer()(asm))
        }
      }*/
    }
  }
}
