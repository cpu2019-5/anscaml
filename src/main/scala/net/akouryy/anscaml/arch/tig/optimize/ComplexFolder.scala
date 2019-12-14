package net.akouryy.anscaml
package arch.tig
package optimize

import asm._
import Branch.{CondV, CondVC}
import base._

import scala.collection.mutable

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
        case (Block(_, tLines3, _, tji3), Block(_, fLines3, _, fji3)) if tji3 == fji3 =>
          (tLines3, fLines3) match {
            case (Nil, List(Line(_, fRet2, BinOpVTree(FnegCond, fArg2, XReg.C_MINUS_ONE)))) =>
              Some((fArg2, fArg2, fRet2, !negativeArg1ToTru, tji3))
            case (List(Line(_, tRet2, BinOpVTree(FnegCond, tArg2, XReg.C_MINUS_ONE))), Nil) =>
              Some((tArg2, tRet2, tArg2, negativeArg1ToTru, tji3))
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

  private[this] def foldMovesInStrictBlock(fun: FDef) = {
    val env = mutable.Map[XVar, XVar]()

    def wrap(x: XID) = x.fold(env.getOrElse(_, x), identity)

    for ((bi, b) <- fun.blocks) {
      env.clear()
      fun.blocks(bi) = b.copy(lines = b.lines.map { oldLine =>
        val line = oldLine.copy(inst = oldLine.inst.mapXID(wrap))
        line match {
          case Line(_, dest: XVar, Mv(src: XVar)) => env(dest) = src
          case Line(_, _, _: CallDir) => env.clear()
          case Line(_, _, inst) =>
        }
        line
      })
      fun.jumps(b.output) = fun(b.output) match {
        case j: StartFun => !!!!(j)
        case j @ Return(_, _, value, _) => j.copy(value = wrap(value))
        case j @ Branch(_, _, CondVC(op, left, right), _, _, _) =>
          j.copy(cond = CondVC(op, wrap(left), right.mapV(wrap)))
        case j @ Branch(_, _, CondV(op, left, right), _, _, _) =>
          j.copy(cond = CondV(op, wrap(left), wrap(right)))
        case j @ Merge(_, _, inputs, _, _) =>
          j.copy(inputs = inputs.map(m => m.copy(xid = wrap(m.xid))))
      }
    }
  }

  def apply(program: Program): Boolean = {
    var changed = false

    for (f <- program.functions) {
      val oldBlocks = f.blocks.toMap
      val oldJumps = f.jumps.toMap

      foldFNegCond(f)
      foldMovesInStrictBlock(f)

      changed ||= oldBlocks != f.blocks || oldJumps != f.jumps
    }

    changed
  }
}
