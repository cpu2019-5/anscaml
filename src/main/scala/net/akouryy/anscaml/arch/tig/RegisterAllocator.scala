package net.akouryy.anscaml
package arch.tig

import asm._
import base._

import scala.collection.mutable

class RegisterAllocator {

  type IGraph = Map[XVar, Set[XVar]]

  private[this] var liveness: analyze.Liveness.Info = _
  private[this] var regCntMax: Int = _
  private[this] var regCntSum: Int = _

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

  private[this] def allocateInFun(
    f: FDef, interference: IGraph, // preferences: Map[XVar, Map[XID, Int]],
  ): Map[XID, XReg] = {
    val regEnv = XReg.VALID_REGS.map(r => (r: XID) -> r).to(mutable.Map)

    def allocate(xid: XID) = xid match {
      case _: XReg =>
      case v: XVar =>
        assert(!regEnv.contains(v))
        val used = interference.getOrElse(v, Set()).flatMap(regEnv.get)
        regEnv(v) = XReg.NORMAL_REGS.find(r => !used.contains(r)).getOrElse(
          ????(s"Spill: $v vs ${interference(v)}")
        )
        if (regEnv(v).id >= 38)
          println(v, interference.getOrElse(v, Set()),
            interference.getOrElse(v, Set()).map(regEnv.get))
        regEnv(v)
    }

    f.args.foreach(allocate)

    for (block <- f.blocks.values) {
      f(block.input) match {
        case j: Merge => allocate(j.outputID)
        case _ =>
      }
      for (Line(_, dest, _) <- block.lines) {
        allocate(dest)
      }
    }

    regEnv.toMap
  }

  private[this] def applyAllocation(f: FDef, regEnv: Map[XID, XReg]): FDef = {
    def wrap(xid: XID): XReg = xid.fold(regEnv, identity)

    def wrapVC(vc: VC): VC = vc.mapV(wrap)

    val newChart = new Chart

    for (b <- f.blocks.values) {
      newChart.blocks(b.i) = b.copy(lines = b.lines.zipWithIndex.map { case (line, lineIndex) =>
        val newInst = line.inst match {
          case CallDir(fn, args, None) =>
            val saves =
              (liveness(b.i)(lineIndex + 1 /* 出口生存 */) -- line.dest.asXVar)
                .map(v => v.id -> regEnv(v)).toMap
            CallDir(fn, args, Some(saves))
          case inst => inst
        }
        Line(line.comment, wrap(line.dest), newInst.mapXID(wrap))
      })
    }

    for (j <- f.jumps.values) {
      newChart.jumps(j.i) = j match {
        case j: StartFun => j
        case Branch(cm, _, expr, input, tru, fls) =>
          Branch(cm, j.i, expr.mapLR(wrap)(wrapVC, wrap), input, tru, fls)
        case Merge(cm, _, inputs, outputID, output) =>
          val newInputs = inputs.map(_.mapXID(wrap))
          Merge(cm, j.i, newInputs, wrap(outputID), output)
        case Return(cm, _, value, input) => Return(cm, j.i, wrap(value), input)
      }
    }

    FDef(f.name, f.args.map(wrap), newChart, f.typ, f.info)
  }

  def apply(program: Program, liveness: analyze.Liveness.Info): Program = {
    this.liveness = liveness
    regCntMax = 0
    regCntSum = 0

    val res = Program(
      program.gcSize,
      program.tyEnv,
      program.functions.map { f =>
        val interference = interferenceGraph(f)
        val regEnv = allocateInFun(f, interference)

        val regCnt = regEnv.flatMap {
          case (_: XVar, reg) => Some(reg.id)
          case _ => None
        }.maxOption.getOrElse(0)

        regCntMax = regCntMax max regCnt
        regCntSum += regCnt

        applyAllocation(f, regEnv)
      },
    )

    println(s"[RA] max: $regCntMax, ave: ${
      regCntSum * 1.0 / program.functions.length
    }")

    res
  }
}
