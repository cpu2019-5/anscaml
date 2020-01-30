package net.akouryy.anscaml
package arch.tig
package emit

import asm._
import base._

/**
  * optimizes and formats asm programs to make it easier for Emitter to handle them. LastOptimizer:
  *
  * <ul><li>
  * arranges parameter registers at the top of function definitions,
  * </li><li>
  * moves return values to the return register,
  * </li><li>
  * and converts merge operation of Merge and ForLoopTop to move in the preceding blocks.
  * </li></ul>
  */
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
          case ForLoopTop(cm, _, cond, negated, merges, input, loopBottom, body, kont) =>
            val bottom = f.body.jumps(loopBottom).asInstanceOf[ForLoopBottom]
            updateLines(input, lines => lines ++ Emitter.moveSimultaneously(merges.flatMap {
              case ForLoopVar(in, _, loop) => Option.when(in != loop) {
                Emitter.Move(src = in.asXReg.get, dest = loop.asXReg.get)
              }
            }).map { case Emitter.Move(s, d) => Line(NC, d, Mv(s)) })
            updateLines(bottom.input, lines => lines ++ Emitter.moveSimultaneously(merges.flatMap {
              case ForLoopVar(_, upd, loop) => Option.when(upd != loop) {
                Emitter.Move(src = upd.asXReg.get, dest = loop.asXReg.get)
              }
            }).map { case Emitter.Move(s, d) => Line(NC, d, Mv(s)) })
            ForLoopTop(cm, ji, cond, negated, Nil, input, loopBottom, body, kont)
          case j: ForLoopBottom => j
        }
      }

      f.copy(args = (1 to f.args.size).map(XReg(_)).toList)
    })
}
