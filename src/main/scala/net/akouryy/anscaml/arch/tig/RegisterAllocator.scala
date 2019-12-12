package net.akouryy.anscaml
package arch.tig

import asm._
import base._

import scala.collection.mutable

class RegisterAllocator {

  type IGraph = Map[XVar, Set[XVar]]

  private[this] var liveness: analyze.Liveness.Info = _

  private[this] def interferenceGraph(f: FDef): IGraph = {
    val g = mutable.Map[XVar, Set[XVar]]().withDefault(_ => Set())
    for {
      bi <- f.body.blocks.keysIterator
      live <- liveness(bi)
      v <- live
      w <- live
      if v != w
    } g(v) += w
    g.toMap
  }

  private[this] def allocateInFun(f: FDef, interference: IGraph): FDef = {
    val newChart = new Chart
    val regEnv = XReg.VALID_REGS.map(r => (r: XID) -> r).to(mutable.Map)

    def allocate(xid: XID): XReg = xid match {
      case xid: XReg => xid
      case v: XVar =>
        assert(!regEnv.contains(v))
        val used = interference.getOrElse(v, Set()).flatMap(regEnv.get)
        regEnv(v) = XReg.NORMAL_REGS.find(r => !used.contains(r)).getOrElse(
          throw new NotImplementedError(s"Spill: $v vs ${interference(v)}")
        )
        regEnv(v)
    }

    def getOrAllocate(xid: XID): XReg = regEnv.getOrElse(xid, allocate(xid))

    def wrapVC(vc: VC): VC = vc.fold(v => V(getOrAllocate(v)), _ => vc)

    for ((_ -> Return(cm, ji, value, input)) <- f.body.jumps) {
      newChart.jumps(ji) = Return(cm, ji, allocate(value), input)
    }

    for (block <- f.body.blocks.values.toSeq.reverseIterator) {
      val newLines = block.lines.zipWithIndex.reverseIterator.map {
        case (Line(cm, dest, inst), lineIndex) =>
          val newDest = regEnv(dest)
          val newInst = inst match {
            case CallDir(fn, args, None) =>
              val saves =
                (liveness(block.i)(lineIndex + 1 /* 出口生存 */) -- dest.asXVar)
                  .map(v => v.id -> regEnv(v)).toMap
              CallDir(fn, args, Some(saves))
            case _ => inst
          }
          Line(cm, newDest, newInst.mapXID(getOrAllocate))
      }.toList.reverse
      newChart.blocks(block.i) = Block(block.i, newLines, block.input, block.output)

      val ji1 = block.input
      val newJump = f.body.jumps(ji1) match {
        case j1: StartFun => Some(j1)
        case j1: Return => !!!!(j1)
        case Branch(cm, _, expr, input, tru, fls) =>
          if (newChart.blocks.contains(tru) && newChart.blocks.contains(fls)) {
            Some(Branch(cm, ji1, expr.mapLR(getOrAllocate)(wrapVC, getOrAllocate), input, tru, fls))
          } else {
            None // newJumpの追加はtruとflsの両方が処理された後に行う
          }
        case Merge(cm, _, inputs, outputID, output) =>
          val newInputs = inputs.map { case (xid, bi) => (allocate(xid), bi) }
          Some(Merge(cm, ji1, newInputs, regEnv(outputID), output))
      }
      newJump.foreach(newChart.jumps(ji1) = _)
    }

    FDef(f.name, f.args.map(regEnv.getOrElse(_, XReg.DUMMY)), newChart, f.typ)
  }

  def apply(program: Program, liveness: analyze.Liveness.Info): Program = {
    this.liveness = liveness

    Program(
      program.gcSize,
      program.tyEnv,
      program.functions.map { f =>
        val interference = interferenceGraph(f)
        allocateInFun(f, interference)
      },
    )
  }
}
