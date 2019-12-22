package net.akouryy.anscaml
package knorm
package optimize

object Optimizer {
  def apply(iterationCount: Int, kNorm0: KNorm): KNorm = {
    var kNorm = kNorm0
    for (i <- 0 until iterationCount) {
      Logger.log("KO", s"iteration $i")
      val k = new PeepHole()(Eliminator(new Inliner()(new TypFolder()(Assoc(kNorm)))))
      Logger.log("KO", s"iteration $i")
      if (k == kNorm) return kNorm
      kNorm = k
    }
    kNorm
  }
}
