package net.akouryy.anscaml
package arch.tig
package emit

import asm._
import base._

import scala.annotation.tailrec

object EmitUtil {
  @tailrec def nextNonEmptyBlockIndex(c: Chart, bi0: BlockIndex): BlockIndex = {
    c.blocks(bi0) match {
      case Block(_, _ :: _, _, _) => bi0
      case Block(_, Nil, _, ji1) =>
        c.jumps(ji1) match {
          case j: StartFun => !!!!(j)
          case _: Return | _: Branch | _: ForLoopTop | _: ForLoopBottom => bi0
          case j @ Merge(_, _, _, o, bi2) =>
            if (o != XReg.DUMMY) !!!!(j)
            nextNonEmptyBlockIndex(c, bi2)
        }
    }
  }
}
