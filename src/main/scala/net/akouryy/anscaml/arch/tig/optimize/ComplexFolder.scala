package net.akouryy.anscaml
package arch.tig
package optimize

import asm._
import Branch.{CondV, CondVC}
import base._

object ComplexFolder {

  private[this] def foldFNegCond(fun: FDef) = {
    for {
      bi0 <- fun.blocks.keysIterator // 0: 上
      b0 <- fun.get(bi0)
      Branch(_, ji1, cond1, _, tbi2, fbi2) <- Some(fun(b0.output))
      (arg1, negativeArg1ToTru) <- cond1 match {
        case CondVC(Le, arg1, C(Word(-1))) => Some((arg1, true))
        case CondV(FLe, XReg.ZERO, arg1) => Some((arg1, false))
        case _ => None
      }
      (arg2, tRet2, fRet2, isResultCorrect, ji3) <- (fun(tbi2), fun(fbi2)) match {
        case (Block(_, List(tLine3), _, tji3), Block(_, List(fLine3), _, fji3)) if tji3 == fji3 =>
          (tLine3, fLine3) match {
            case (
              Line(_, tRet2, Mv(tArg2)),
              Line(_, fRet2, BinOpVTree(FnegCond, fArg2, XReg.C_MINUS_ONE)),
              ) if tArg2 == fArg2 =>
              Some((tArg2, tRet2, fRet2, !negativeArg1ToTru, tji3))
            case (
              Line(_, tRet2, BinOpVTree(FnegCond, tArg2, XReg.C_MINUS_ONE)),
              Line(_, fRet2, Mv(fArg2)),
              ) if tArg2 == fArg2 =>
              Some((tArg2, tRet2, fRet2, negativeArg1ToTru, tji3))
            case _ => None
          }
        case _ => None
      }
      j3 @ Merge(_, _, inputs3, _, _) <- Some(fun(ji3))
      if inputs3.contains(MergeInput(tbi2, tRet2)) && inputs3.contains(MergeInput(fbi2, fRet2))
    } {
      fun.blocks --= Seq(tbi2, fbi2)
      fun.jumps -= ji1

      val newRet = XVar.generate(tRet2.idStr)
      val lines =
        if (isResultCorrect) {
          Seq(Line(CM("[XF] fnegcond"), newRet, BinOpVTree(FnegCond, arg2, arg1)))
        } else { // fnegcond後にもう一度符号反転を入れる必要がある
          val tmp = XVar.generate(tRet2.idStr)
          Seq(
            Line(NC, tmp, BinOpVTree(FnegCond, arg2, arg1)),
            Line(CM("[XF] fnegcond"), newRet, BinOpVTree(FnegCond, tmp, XReg.C_MINUS_ONE)),
          )
        }

      fun.blocks(bi0) = b0.copy(
        lines = b0.lines ++ lines,
        output = ji3,
      )
      fun.jumps(ji3) = j3.copy(
        inputs = inputs3.flatMap(m =>
          if (m.bi == tbi2) Some(MergeInput(bi0, newRet))
          else if (m.bi == fbi2) None
          else Some(m)
        ),
      )
    }
  }

  def apply(program: Program): Boolean = {
    var changed = false

    for (f <- program.functions) {
      val oldBlocks = f.blocks.toMap
      val oldJumps = f.jumps.toMap

      foldFNegCond(f)

      changed ||= oldBlocks != f.blocks || oldJumps != f.jumps
    }

    changed
  }
}
