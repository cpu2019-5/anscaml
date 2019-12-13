package net.akouryy.anscaml
package arch.tig
package optimize

import asm._

import scala.collection.mutable

object MergeMerge {
  def apply(program: Program): Boolean = {
    var changed = false
    for (f <- program.functions) {
      for ((_, m0 @ Merge(_, ji0, inputs0, _, _)) <- f.body.jumps) { // 0が下、3が上
        val newInputs0 = inputs0.map(_.toPair).to(mutable.SortedMap)
        for {
          MergeInput(bi1, toMerge0) <- inputs0
          Block(_, Nil, ji2, _) <- Some(f.body.blocks(bi1))
          Merge(_, _, inputs2, merged2, _) <- Some(f.body.jumps(ji2))
        } {
          newInputs0 -= bi1
          f.body.blocks -= bi1
          f.body.jumps -= ji2
          for (MergeInput(bi3, toMerge2) <- inputs2) {
            f.body.blocks(bi3) = f.body.blocks(bi3).copy(output = ji0)
            // outputID0に行くID
            newInputs0(bi3) = if (merged2 == toMerge0) toMerge2 else toMerge0
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
