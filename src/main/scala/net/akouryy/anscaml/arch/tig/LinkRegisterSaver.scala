package net.akouryy.anscaml
package arch.tig

import asm._
import base._

object LinkRegisterSaver {
  def apply(program: Program): Unit = {
    for (f <- program.functions; if f.body.blocks.nonEmpty) {
      val lrVar = XVar.generate(ID.Special.RA_LINK_REG)
      locally {
        val sbi = f.body.blocks.firstKey
        val sb = f.body.blocks(sbi)
        f.body.blocks(sbi) = sb.copy(
          lines = Line(CM("[LRS] save LR"), lrVar, Mv(XReg.LINK)) +: sb.lines
        )
      }
      for (Return(_, _, _, rbi) <- f.body.jumps.valuesIterator) {
        val rb = f.body.blocks(rbi)
        val restore = Line(CM("[LRS] restore LR"), XReg.LINK, Mv(lrVar))
        f.body.blocks(rbi) = rb.copy(
          lines = rb.lines match {
            case init :+ (last @ Line(_, _, _: CallDir)) =>
              init :+ restore :+ last
            case lines =>
              lines :+ restore
          }
        )
      }
    }
  }
}
