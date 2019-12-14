package net.akouryy.anscaml.arch.tig
package optimize

import asm._

object AliasSolver {
  def apply(asm: Program): Unit = {
    for (f <- asm.functions) {
      val aliases =
        (for {
          b <- f.blocks.values
          Line(_, dest: XVar, Alias(src: XVar)) <- b.lines
        } yield {
          (dest, src)
        }).toMap

      def wrap(x: XID) = x.fold(v => aliases.getOrElse(v, v), identity)

      f.blocks.mapValuesInPlace { (_, b) =>
        b.copy(lines = b.lines.flatMap { l =>
          Option.unless(l.inst.isInstanceOf[Alias])(l.copy(inst = l.inst.mapXID(wrap)))
        })
      }

      f.jumps.mapValuesInPlace { (_, j) =>
        j match {
          case _: StartFun => j
          case j: Return => j.copy(value = wrap(j.value))
          case j: Branch => j.copy(cond = j.cond match {
            case Branch.CondVC(op, left, right) => Branch.CondVC(op, wrap(left), right.mapV(wrap))
            case Branch.CondV(op, left, right) => Branch.CondV(op, wrap(left), wrap(right))
          })
          case j: Merge => j.copy(inputs = j.inputs.map(i => MergeInput(i.bi, wrap(i.xid))))
        }
      }
    }
  }
}
