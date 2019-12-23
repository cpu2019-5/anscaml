package net.akouryy.anscaml
package arch.tig
package optimize

import asm._
import base._

import scala.collection.mutable

/**
  * 0〜1行のブロックを挟んでMergeが続いているとき、それを合併する
  */
object MergeMerge {
  def apply(program: Program): Boolean = {
    var changed = false
    for (f <- program.functions) {
      // 0が下、3が上
      for ((_, m0 @ Merge(_, ji0, inputs0, _, _)) <- f.body.jumps if inputs0.sizeIs >= 2) {
        val newInputs0 = inputs0.map(_.toPair).to(mutable.SortedMap)
        for {
          MergeInput(bi1, phiArg0) <- inputs0
          Block(_, lines1, ji2, _) <- Some(f.body.blocks(bi1))
          if lines1.sizeIs <= 1
          lineOpt1 = lines1.headOption
          Merge(_, _, inputs2, phiRes2, _) <- Some(f.body.jumps(ji2))
          if lineOpt1.forall(line1 => line1.dest == phiArg0 && phiRes2 == XReg.DUMMY)
        } {
          /*
            1. line1がないとき
              - Merge0とMerge2のφ関数(phi0とphi2)がどちらもあるとき、phi2の返り値がphi0の引数になっている
                (そうでない場合はphi2は不要である)
            2. line1があって、そのdestがphi0の引数に等しく、phi2がないとき
              - phiNew0の引数newPhiArg0を新たに作る
              - line1のdestを新たな変数newPhiArg0に変える
              //// - line1のinstのうちphi2の返り値だった部分をphi2の引数に変える
              - 元の2からφを継承する必要がない
          */
          newInputs0 -= bi1
          f.body.blocks -= bi1
          f.body.jumps -= ji2
          for (MergeInput(bi3, phiArg2) <- inputs2) {
            val b3 = f.body.blocks(bi3)

            lineOpt1 match {
              case None =>
                f.body.blocks(bi3) = b3.copy(output = ji0)
                // phiNew0の引数
                newInputs0(bi3) = if (phiRes2 == phiArg0) phiArg2 else phiArg0
              case Some(line1) if line1.dest == phiArg0 && phiRes2 == XReg.DUMMY =>
                val newPhiArg0 = XVar.generate(line1.dest.idStr)
                f.body.blocks(bi3) = b3.copy(
                  output = ji0,
                  lines = b3.lines :+ line1.copy(dest = newPhiArg0),
                )
                newInputs0(bi3) = newPhiArg0
              case _ => ????(lineOpt1)
            }
          }
          changed = true
        }
        if (changed) {
          f.body.jumps(ji0) = m0.copy(inputs = newInputs0.toList.map(MergeInput.tupled))
        }
      }
    }
    changed
  }

}
