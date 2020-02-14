package net.akouryy.anscaml
package arch.tig

import asm._
import base._

object LinkRegisterSaver {
  def apply(program: Program): Unit = {
    for (f <- program.functions; if !f.info.isLeaf && f.body.blocks.nonEmpty) {
      val lrVar = XVar.generate(ID.Special.RA_LINK_REG)
      locally {
        val sbi = f.body.blocks.firstKey
        val sb = f.body.blocks(sbi)
        f.body.blocks(sbi) = sb.copy(
          lines = Line(CM("[LRS] save LR"), lrVar, Mv(XReg.LINK)) +: sb.lines
        )
      }
      for (ret @ Return(_, ji, _, None, rbi) <- f.body.jumps.valuesIterator) {
        val rb = f.body.blocks(rbi)
        rb.lines match {
          case init :+ (last @ Line(_, _, _: CallDir)) =>
            f.body.blocks(rbi) =
              rb.copy(lines = init :+ Line(CM("[LRS] restore LR"), XReg.LINK, Mv(lrVar)) :+ last)
          case _ =>
            f.body.jumps(ji) = ret.copy(address = Some(lrVar))
        }

      }
    }
  }
}
