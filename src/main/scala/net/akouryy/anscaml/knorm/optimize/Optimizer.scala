package net.akouryy.anscaml.knorm
package optimize

object Optimizer {
  def apply(iterationCount: Int, kNorm0: KNorm): KNorm = {
    var kNorm = kNorm0
    for (i <- 0 until iterationCount) {
      println(s"[KOptimize] iteration $i")
      val k = new TypFolder()(Assoc(kNorm))
      if (k == kNorm) return kNorm
      kNorm = k
    }
    kNorm
  }
}
