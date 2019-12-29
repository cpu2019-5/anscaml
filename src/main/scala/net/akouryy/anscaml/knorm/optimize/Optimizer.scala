package net.akouryy.anscaml
package knorm
package optimize

import base._

object Optimizer {
  def apply(iterationCount: Int, kNorm0: KNorm): KNorm = {
    var kNorm = kNorm0
    for (i <- 0 until iterationCount) {
      Logger.log("KO", s"iteration $i")
      var k = kNorm
      k = Assoc(k)
      k = new TypFolder()(k)
      k = new Inliner()(k)
      k = Eliminator(k)
      k = new PeepHole()(k)
      Logger.log("KO", s"iteration $i")
      if (k == kNorm) return kNorm
      kNorm = k

      /*if(i == 10) {
        scala.util.Using.resource(new java.io.PrintWriter("../temp/a.txt"))(base.PPrinter.writeTo
        (_, k))
      }*/
    }
    kNorm
  }
}
