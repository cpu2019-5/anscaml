package net.akouryy.anscaml
package arch.tig
package optimize

import asm._

import scala.collection.mutable

object MergeMerge {
  def apply(program: Program): Boolean = {
    var changed = false
    for (f <- program.functions) {
      for ((_, m0 @ Merge(_, ji0, inputs0, _, _)) <- f.body.jumps) {
        val newInputs0 = inputs0.map(_.swap).to(mutable.SortedMap)
        for {
          (inputID0, bi1) <- inputs0
          Block(_, Nil, ji2, _) <- Some(f.body.blocks(bi1))
          Merge(_, _, inputs2, outputID2, _) <- Some(f.body.jumps(ji2))
          if inputID0 == outputID2
        } {
          newInputs0 -= bi1
          f.body.blocks -= bi1
          f.body.jumps -= ji2
          for ((inputID2, bi3) <- inputs2) {
            f.body.blocks(bi3) = f.body.blocks(bi3).copy(output = ji0)
            newInputs0(bi3) = inputID2
          }
          changed = true
        }
        if (changed) {
          f.body.jumps(ji0) = m0.copy(inputs = newInputs0.toList.map(_.swap))
        }
      }
    }
    changed
  }

}
