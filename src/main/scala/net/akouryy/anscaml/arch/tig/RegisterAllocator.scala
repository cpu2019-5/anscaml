package net.akouryy.anscaml
package arch.tig

import asm._
import base._

import scala.collection.{immutable, mutable}

class RegisterAllocator {

  import RegisterAllocator._

  type IGraph = Map[XVar, Set[XID]]

  private[this] var liveness: analyze.Liveness.Info = _
  private[this] val safeRegsMap =
    mutable.Map[String, immutable.SortedSet[XReg]]()
  private[this] var currentSafeRegs: immutable.SortedSet[XReg] = _
  private[this] var regCntMax: Int = _
  private[this] var regCntSum: Int = _

  private[this] def interferenceGraph(f: FDef, avoidCallerSave: Boolean): IGraph = {
    val g = mutable.Map[XVar, Set[XID]]().withDefault(_ => Set())
    for {
      bi <- f.body.blocks.keysIterator
      live <- liveness(bi)
      v <- live
      w <- live
      if v != w
    } g(v) += w
    if (avoidCallerSave) {
      for {
        b <- f.body.blocks.valuesIterator
        liveList = liveness(b.i)
        ((liveIn, liveOut), Line(_, _, CallDir(callee, _, _)))
          <- liveList.zip(liveList.tail).zipStrict(b.lines)
        v <- liveIn & liveOut
        w <- XReg.NORMAL_REGS_SET --
             safeRegsMap.getOrElse(callee, throw new SpillError(s"unsafe function $callee"))
      } g(v) += w
    }
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
        val used = interference.getOrElse(v, Set()).flatMap(_.fold(regEnv.get, Some(_)))
        regEnv(v) = XReg.NORMAL_REGS.find(r => !used.contains(r)).getOrElse(
          throw new SpillError(s"Spill: $v vs ${interference(v)}")
        )
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

    currentSafeRegs = XReg.NORMAL_REGS_SET

    val newChart = new Chart

    for (b <- f.blocks.values) {
      newChart.blocks(b.i) = b.copy(lines = b.lines.zipWithIndex.map { case (line, lineIndex) =>
        val newInst = line.inst match {
          case CallDir(callee, args, None) =>
            if (callee != f.name) { // 再帰呼び出しはsafeRegsに関与しない
              currentSafeRegs &= safeRegsMap.getOrElse(callee, Set())
            }
            val calleeSafeRegs: Set[XReg] = safeRegsMap.getOrElse(callee, Set())
            val saves =
              (liveness(b.i)(lineIndex + 1 /* 出口生存 */) -- line.dest.asXVar)
                .flatMap { v =>
                  val r: XReg = regEnv(v)
                  Option.unless(calleeSafeRegs contains r)(v.id -> r)
                }.toMap
            CallDir(callee, args, Some(saves))
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

    FDef(
      f.name, f.args.map(wrap), newChart, f.typ,
      f.info.copy(safeRegs =
        currentSafeRegs -- regEnv.filter(_._1.isInstanceOf[XVar]).values.to(immutable.SortedSet)
      ),
    )
  }

  def apply(program: Program, liveness: analyze.Liveness.Info): Program = {
    this.liveness = liveness
    safeRegsMap.clear()
    regCntMax = 0
    regCntSum = 0

    val res = Program(
      program.gcSize,
      program.tyEnv,
      program.functions.mapInReversedOrder { f =>
        val regEnv = try {
          allocateInFun(f, interferenceGraph(f, avoidCallerSave = true))
        } catch {
          case e: SpillError =>
            println(s"[RA] ${f.name}: ${e.getMessage}")
            allocateInFun(f, interferenceGraph(f, avoidCallerSave = false))
        }

        val regCnt = regEnv.flatMap {
          case (_: XVar, reg) => Some(reg.id)
          case _ => None
        }.toSet.size

        regCntMax = regCntMax max regCnt
        regCntSum += regCnt

        val newF = applyAllocation(f, regEnv)

        safeRegsMap(newF.name) = newF.info.safeRegs
        // println(newF.name, XReg.toRangeString(newF.info.safeRegs))

        newF
      },
    )

    println(s"[RA] max: $regCntMax, ave: ${
      regCntSum * 1.0 / program.functions.length
    }")

    res
  }
}

object RegisterAllocator {

  private final class SpillError(message: String) extends RuntimeException(message)

}
