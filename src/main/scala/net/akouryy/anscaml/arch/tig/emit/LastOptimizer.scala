package net.akouryy.anscaml.arch.tig
package emit

import asm._

object LastOptimizer {
  def apply(program: Program): Program = {
    program.copy(functions = program.functions.map { f =>
      def updateLines(bi: BlockIndex, fn: List[Line] => List[Line]) = {
        val b = f.body.blocks(bi)
        f.body.blocks(bi) = b.copy(lines = fn(b.lines))
      }

      updateLines(f.body.blocks.firstKey, lines =>
        Emitter.moveSimultaneously(f.args.zipWithIndex.map {
          case (a, i) => Emitter.Move(src = XReg.NORMAL_REGS(i), dest = a)
        }) ::: lines
      )

      f.body.jumps.mapValuesInPlace { (ji, j) =>
        j match {
          case _: StartFun | _: Condition => j
          case Return(_, src, bi) =>
            if (src == XReg.REG_DUMMY) {
              j
            } else {
              if (src != XReg.REG_RETURN) {
                updateLines(bi, lines => lines :+ Line(XReg.REG_RETURN, Mv(src)))
              }
              Return(ji, XReg.REG_RETURN, bi)
            }
          case Merge(_, inputs, dest, obi) =>
            Merge(ji, inputs.map {
              case (inputXID, ibi) =>
                if (inputXID != dest) {
                  updateLines(ibi, lines => lines :+ Line(dest, Mv(inputXID)))
                }
                (XReg.REG_DUMMY, ibi)
            }, XReg.REG_DUMMY, obi)
        }
      }

      f.body.blocks.mapValuesInPlace { (_, b) =>
        b.copy(lines = b.lines.filter {
          case Line(x, Mv(y)) if x == y => false
          case _ => true
        })
      }

      f.copy(args = (1 to f.args.size).map(XReg(_)).toList)
    })
  }
}
