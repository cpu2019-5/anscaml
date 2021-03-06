package net.akouryy.anscaml
package arch.tig
package optimize

import analyze.Liveness
import asm._
import Branch.{Cond, CondVC}
import base._

import scala.collection.mutable

object ComplexFolder {
  private[this] def foldSelect(fun: FDef) = {
    for {
      b0 <- fun.blocks.valuesIterator // 0: 上
      Branch(_, ji1, cond1, _, tbi2, fbi2) <- Some(fun(b0.output))
      Block(_, Nil, _, ji3) <- Some(fun(tbi2))
      Block(_, Nil, _, `ji3`) <- Some(fun(fbi2))
      Util.The(j3: Merge) <- Some(fun(ji3))
      if j3.outputID != XReg.DUMMY
    } {
      val tmp = XVar.generate("$sel")
      var tru, fls = null: XID
      val newInputs = j3.inputs.flatMap {
        case MergeInput(`tbi2`, x) => tru = x; None
        case MergeInput(`fbi2`, x) => fls = x; Some(MergeInput(b0.i, tmp))
        case m => Some(m)
      }
      fun.blocks(b0.i) = b0.copy(
        output = ji3,
        lines = b0.lines :+ Line(NC, tmp, Select(cond1, tru, fls))
      )
      fun.jumps -= ji1
      fun.blocks --= Seq(tbi2, fbi2)
      fun.jumps(ji3) = j3.copy(inputs = newInputs)
    }
  }

  private[this] def foldFNegCond(fun: FDef) = {
    for {
      bi0 <- fun.blocks.keysIterator // 0: 上
      b0 <- fun.get(bi0)
      Branch(_, ji1, cond1, _, tbi2, fbi2) <- Some(fun(b0.output))
      (arg1, negativeArg1ToTru) <- cond1 match {
        case CondVC(Le, arg1, C(Word(-1)) | V(XReg.C_MINUS_ONE)) => Some((arg1, true))
        case Cond(Le | FLe, XReg.ZERO, V(arg1)) => Some((arg1, false))
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
      val lastMoves = mutable.ListBuffer[Line]()
      fun.blocks(bi) = b.copy(lines =
        b.lines.flatMap { oldLine =>
          val line = oldLine.copy(inst = oldLine.inst.mapXID(wrap))
          line match {
            case Line(_, dest: XVar, Mv(src: XVar)) =>
              env(dest) = src
              lastMoves += line
              Nil
            case Line(_, _, _: CallDir) =>
              env.clear()
              val res = (lastMoves += line).toList
              lastMoves.clear()
              res
            case _ => List(line)
          }
        }
        ++ lastMoves
      )
      fun.jumps(b.output) = fun(b.output) match {
        case j: StartFun => !!!!(j)
        case j @ Return(_, _, value, _, _) => j.copy(value = wrap(value))
        case j @ Branch(_, _, cond, _, _, _) =>
          j.copy(cond = cond.mapLR(wrap)(_.mapV(wrap), wrap))
        case j @ Merge(_, _, inputs, _, _) =>
          j.copy(inputs = inputs.map(m => m.copy(xid = wrap(m.xid))))
        case j @ ForLoopTop(_, _, cond, _, merges, _, _, _, _) =>
          j.copy(
            cond = cond.mapLR(wrap)(_.mapV(wrap), wrap),
            merges = merges.map { case ForLoopVar(in, upd, loop) =>
              ForLoopVar(in = wrap(in), upd = wrap(upd), loop = loop)
            }
          )
        case j: ForLoopBottom => j
      }
    }
  }

  private[this] def foldCommonInstAfterBranch(fun: FDef) = {
    for {
      Branch(_, _, _, bi0, tbi2, fbi2) <- fun.jumps.valuesIterator // 0: 上
      tb2 @ Block(_, Line(tcm, tDest2: XVar, commonInst) :: tLines2, _, _) <- Some(fun(tbi2))
      fb2 @ Block(_, Line(fcm, fDest2: XVar, `commonInst`) :: fLines2, _, _) <- Some(fun(fbi2))
    } {
      val newDest = XVar.generate(tDest2.idStr)
      fun(bi0) :+= Line((tcm :+ "[XF] if-common") + fcm, newDest, commonInst)
      if (tLines2.sizeCompare(fLines2) >= 0) {
        fun.blocks(tbi2) = tb2.copy(lines = Line(NC, tDest2, Mv(newDest)) :: tLines2)
        fun.blocks(fbi2) = fb2.copy(lines = Line(NC, fDest2, Mv(newDest)) :: fLines2)
      } else {
        fun.blocks(tbi2) = tb2.copy(lines = Line(NC, tDest2, Mv(newDest)) :: tLines2)
        fun.blocks(fbi2) = fb2.copy(lines = Line(NC, fDest2, Mv(newDest)) :: fLines2)
      }
    }
  }

  private[this] def foldSingleMerge(fun: FDef) = {
    for {
      Merge(_, ji1, List(MergeInput(bi0, in1)), out1, bi2) <- fun.jumps.valuesIterator
    } {
      val b0 = fun(bi0)
      val b2 = fun(bi2)
      val ji3 = b2.output
      val j3 = fun(ji3)
      fun.blocks(bi0) = b0.copy(
        lines =
          b0.lines
          ++ Option.when(in1 != out1 && out1 != XReg.DUMMY)(Line(NC, out1, Mv(in1)))
          ::: b2.lines,
        output = ji3,
      )
      fun.jumps -= ji1
      fun.blocks -= bi2
      fun.jumps(ji3) = j3.convertInput(bi2, bi0)
    }
  }

  private[this] def foldEmptyBranch(fun: FDef) = {
    for {
      Branch(_, ji1, _, bi0, tbi2, fbi2) <- fun.jumps.valuesIterator
      Block(_, Nil, _, ji3) <- fun.get(tbi2)
      Block(_, Nil, _, ji3_) <- fun.get(fbi2)
      if ji3 == ji3_
      m3 @ Merge(_, _, List(MergeInput(_, XReg.DUMMY), MergeInput(_, XReg.DUMMY)), XReg.DUMMY, bi4)
        <- fun.get(ji3)
    } {
      // 単独併合はJumpFolderで潰せる
      fun.jumps(ji1) = Merge(NC, ji1, List(MergeInput(bi0, XReg.DUMMY)), XReg.DUMMY, tbi2)
      fun.jumps(ji3) = m3.copy(inputs = List(MergeInput(tbi2, XReg.DUMMY)))
      fun.blocks -= fbi2
    }
  }

  /**
    * {{{let v = Load(...) in (A; if B then C else D(v)) # v is not used in A,B,C. A is pure. }}}
    * ↓
    * {{{A; if B then C else let v = Load(...) in D(v)}}}
    */
  private[this] def eliminatePartiallyUnusedLoad(fun: FDef) = {
    val liveness = Liveness.analyzeFunction(fun)

    for {
      block <- fun.body.blocks.values.toSeq
      Branch(_, _, Cond(_, cLeft, cRight), _, tru, fls) <- Some(fun.body.jumps(block.output))
      (
        line @ Line(_, v: XVar, Mv(XID.Immutable(_)) | Load(XID.Immutable(_), VC.Immutable(_), _)),
        lineIndex
        ) <- block.lines.zipWithIndex
      if block eq fun.body.blocks(block.i)
      if cLeft != v && cRight != V(v) /* v is not used in B */
      livT = liveness(tru).head contains v
      livF = liveness(fls).head contains v
      if livT ^ livF /* v is not used in C (but is in D) */
      followingLines = block.lines.drop(lineIndex + 1)
      if followingLines.forall {
        case Line(_, _, _: Store | _: CallDir) => false /* A is pure */
        case line => !Liveness.useInInst(line.inst).toSet.contains(v) /* v is not used in A */
      }
    } {
      fun.body.blocks(block.i) = block.copy(lines = block.lines.take(lineIndex) ++ followingLines)
      val livIndex = if (livT) tru else fls
      val livBlock = fun.body.blocks(livIndex)
      fun.body.blocks(livIndex) = livBlock.copy(lines = line +: livBlock.lines)
      // Logger.log("CF-EPUL", s"partially eliminated $line in ${fun.name}")
    }
  }

  def apply(program: Program): Boolean = {
    var changed = false

    for (f <- program.functions) {
      val oldBlocks = f.blocks.toMap
      val oldJumps = f.jumps.toMap

      foldSelect(f)
      foldFNegCond(f)
      foldMovesInStrictBlock(f)
      foldCommonInstAfterBranch(f)
      foldSingleMerge(f)
      foldEmptyBranch(f)
      eliminatePartiallyUnusedLoad(f)

      changed ||= oldBlocks != f.blocks || oldJumps != f.jumps
    }

    changed
  }
}
