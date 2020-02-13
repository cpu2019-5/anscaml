package net.akouryy.anscaml
package arch.tig

import asm._
import base._

import scala.collection.{immutable, mutable}

final class RegisterAllocator {

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
      live <- liveness(bi) /* ブロック先頭、各行間、ブロック末尾 */
      v <- live
    } g(v) ++= live - v

    /* 「マージ用mv処理→条件判定」の順になるため条件式だけで使用される(body/kontで不要な)変数もループ変数と干渉 */
    for {
      Util.The(flt: ForLoopTop) <- f.body.jumps.valuesIterator
      ForLoopVar(_, _, lv) <- flt.merges
      Branch.Cond(_, l, r) = flt.cond
      cv <- l.asXVar ++ r.asVXVar
    } {
      g(cv) += lv
      lv.asXVar.foreach(g(_) += cv)
    }

    if (avoidCallerSave) {
      for {
        b <- f.body.blocks.valuesIterator
        liveList = liveness(b.i)
        ((liveIn, liveOut), Line(_, _, CallDir(callee, _, _)))
          <- liveList.zip(liveList.tail).zipStrict(b.lines)
        v <- liveIn & liveOut
      } {
        g(v) ++= XReg.NORMAL_REGS_SET --
                 safeRegsMap.getOrElse(callee, throw new SpillError(s"unsafe function $callee"))
      }
    }
    g.toMap
  }

  private[this] def buildPreferencesBase(f: FDef): mutable.Map[XVar, mutable.Map[XID, Int]] = {
    val pr = mutable.Map[XVar, mutable.Map[XID, Int]]()

    @inline def prefer(v: XVar, x: XID, w: Int) =
      pr.getOrElseUpdate(v, mutable.Map().withDefaultValue(0))(x) += w

    for {
      (bi, b) <- f.blocks
      (line /*, (liveIn, liveOut)*/) <- b.lines //.zipStrict(liveness(bi).zip(liveness(bi).tail))
    } line match {
      // case Line(_, dest: XVar, Mv(src)) => prefer(dest, src, 100)
      case Line(_, _, CallDir(_, args, _)) =>
        for ((a: XVar, i) <- args.zipWithIndex) prefer(a, XReg.NORMAL_REGS(i), 50)
      case _ =>
    }
    for (jump <- f.jumps.valuesIterator) jump match {
      // case Return(_, _, v: XVar, _) => prefer(v, XReg.RETURN, 50)
      // case Merge(_, _, inputs, outputID: XVar, _) =>
      // for(MergeInput(_, xid) <- inputs) prefer(outputID, xid, 30)
      case _ =>
    }
    pr
  }

  private[this] def fixPreference(
    base: mutable.Map[XVar, mutable.Map[XID, Int]], regEnv: mutable.Map[XVar, XReg], v: XVar,
  ): IndexedSeq[XReg] = {
    val pr = mutable.Map[XReg, Int]().withDefaultValue(0)
    for ((x, w) <- base.getOrElse(v, Nil)) x match {
      case x: XVar => pr(regEnv(x)) += w
      case x: XReg =>
        if (XReg.NORMAL_REGS contains x) pr(x) += w
    }
    // if (pr.nonEmpty) println(v, pr.toIndexedSeq.sortBy(_._2).map(_._1))
    pr.toIndexedSeq.sortBy(_._2).map(_._1) ++ XReg.NORMAL_REGS
  }

  private[this] def buildAffinities(f: FDef): Coalescer.Affinities = {
    val aff = mutable.Map[XID, mutable.Map[XID, Int]]()

    @inline def relate(x: XID, y: XID, w: Int) =
      for ((a, b) <- Seq((x, y), (y, x))) {
        aff.getOrElseUpdate(a, mutable.Map().withDefaultValue(0))(b) += w
      }

    for {
      (_, b) <- f.blocks
      line <- b.lines
    } line match {
      case Line(_, x: XVar, Mv(y: XVar)) => relate(x, y, 50)
      case Line(_, x, Mv(y: XReg)) if XReg.NORMAL_REGS_SET contains y => relate(x, y, 50)

      case Line(_, ret, CallDir(_, args, _)) =>
        if (ret match {
          case _: XVar => true
          case ret: XReg => XReg.NORMAL_REGS_SET contains ret
        }) {
          relate(ret, XReg.RETURN, 10)
        }
        for ((a: XVar, i) <- args.zipWithIndex) relate(a, XReg.NORMAL_REGS(i), 50)
      case _ =>
    }

    aff.view.mapValues(_.toMap).toMap
  }

  private[this] def allocateInFun(f: FDef, interference: IGraph): Map[XID, XReg] = {
    val preferencesBase = buildPreferencesBase(f)
    val regEnv = mutable.Map[XVar, XReg]()

    def allocate(xid: XID) = xid match {
      case _: XReg =>
      case v: XVar =>
        assert(!regEnv.contains(v))
        val used = interference.getOrElse(v, Set()).flatMap(_.fold(regEnv.get, Some(_)))

        regEnv(v) = fixPreference(preferencesBase, regEnv, v).find(r => !used.contains(r))
          .getOrElse(throw new SpillError(s"Spill: $v vs ${interference(v)}"))
        regEnv(v)
    }

    f.args.foreach(allocate)

    for (block <- f.blocks.values) {
      f(block.input) match {
        case j: Merge => allocate(j.outputID)
        case j: ForLoopTop if block.i == j.body /* avoid reallocation by kont */ =>
          j.merges.foreach(m => allocate(m.loop))
        case _ =>
      }
      for (Line(_, dest, _) <- block.lines) {
        allocate(dest)
      }
    }

    new Coalescer(interference, buildAffinities(f), regEnv.toMap)()
    //regEnv.toMap
  }

  private[this] def applyAllocation(f: FDef, regEnv: Map[XID, XReg]): FDef = {
    def wrap(xid: XID): XReg = xid.fold(regEnv, identity)

    def wrapVC(vc: VC): VC = vc.mapV(wrap)

    currentSafeRegs = XReg.NORMAL_REGS_SET

    val newChart = new Chart

    for (b <- f.blocks.values) {
      newChart.blocks(b.i) = b.copy(lines =
        b.lines.zipStrict(liveness(b.i).tail).map { case (line, liveOut) =>
          val newInst = line.inst match {
            case CallDir(callee, args, None) =>
              if (callee != f.name) { // 再帰呼び出しはsafeRegsに関与しない
                currentSafeRegs &= safeRegsMap.getOrElse(callee, Set())
              }
              val calleeSafeRegs: Set[XReg] = safeRegsMap.getOrElse(callee, Set())
              val saves = (liveOut -- line.dest.asXVar).flatMap { v =>
                val r: XReg = regEnv(v)
                Option.unless(calleeSafeRegs contains r)(ID(v.idStr) -> r)
              }.toMap
              CallDir(callee, args, Some(saves))
            case inst => inst
          }
          Line(line.comment /* :+ line.toString */ , wrap(line.dest), newInst.mapXID(wrap))
        }
      )
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
        case ForLoopTop(cm, _, cond, negated, merges, input, loopBottom, body, kont) =>
          ForLoopTop(cm, j.i, cond.mapLR(wrap)(wrapVC, wrap), negated, merges.map {
            case ForLoopVar(in, upd, loop) => ForLoopVar(wrap(in), wrap(upd), wrap(loop))
          }, input, loopBottom, body, kont)
        case ForLoopBottom(cm, _, input, loopTop) =>
          ForLoopBottom(cm, j.i, input, loopTop)
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
            Logger.log("RA", s"${f.name}: ${e.getMessage}")
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
        Logger.log("RA", newF.name)

        newF
      },
    )

    Logger.log("RA", s"max: $regCntMax, ave: ${regCntSum * 1.0 / program.functions.length}")

    res
  }
}

object RegisterAllocator {

  private final class SpillError(message: String) extends RuntimeException(message)

}
