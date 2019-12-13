package net.akouryy.anscaml
package arch.tig
package emit

import asm._
import base._

object LastOptimizer {
  def apply(program: Program): Program =
    program.copy(functions = program.functions.map { f =>
      def updateLines(bi: BlockIndex, fn: List[Line] => List[Line]) = {
        val b = f.body.blocks(bi)
        f.body.blocks(bi) = b.copy(lines = fn(b.lines))
      }

      updateLines(f.body.blocks.firstKey, lines => {
        val moveArgs = Emitter.moveSimultaneously(f.args.zipWithIndex.flatMap {
          case (XReg.DUMMY, _) => None
          case (a, i) => Some(Emitter.Move(src = XReg.NORMAL_REGS(i), dest = a.asXReg.get))
        }).map {
          case Emitter.Move(s, d) => Line(NC, d, Mv(s))
        }
        moveArgs ::: lines
      })

      f.body.jumps.mapValuesInPlace { (ji, j) =>
        j match {
          case _: StartFun | _: Branch => j
          case Return(cm, _, src, bi) =>
            if (src == XReg.DUMMY) {
              j
            } else {
              if (src != XReg.RETURN) {
                updateLines(bi, lines => lines :+ Line(NC, XReg.RETURN, Mv(src)))
              }
              Return(cm, ji, XReg.RETURN, bi)
            }
          case Merge(cm, _, inputs, dest, obi) =>
            Merge(cm, ji, inputs.map {
              case MergeInput(ibi, inputXID) =>
                if (inputXID != dest) {
                  updateLines(ibi, lines => lines :+ Line(NC, dest, Mv(inputXID)))
                }
                MergeInput(ibi, XReg.DUMMY)
            }, XReg.DUMMY, obi)
        }
      }

      f.copy(args = (1 to f.args.size).map(XReg(_)).toList)
    })
}
