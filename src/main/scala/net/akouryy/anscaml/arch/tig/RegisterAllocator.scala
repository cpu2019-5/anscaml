package net.akouryy.anscaml
package arch.tig

import asm._

import scala.collection.mutable

class RegisterAllocator {

  type IGraph = Map[AVar, Set[AVar]]

  private[this] var liveness: analyze.Liveness.Info = _

  private[this] def interferenceGraph(f: FDef): IGraph = {
    val g = mutable.Map[AVar, Set[AVar]]().withDefault(_ => Set())
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
    val regEnv = AReg.VALID_REGS.map(r => (r: AID) -> r).to(mutable.Map)

    def allocate(aid: AID): AReg = aid match {
      case aid: AReg => aid
      case v: AVar =>
        assert(!regEnv.contains(v))
        val used = interference.getOrElse(v, Set()).flatMap(regEnv.get)
        regEnv(v) = AReg.NORMAL_REGS.find(r => !used.contains(r)).getOrElse(???)
        regEnv(v)
    }

    def getOrAllocate(aid: AID): AReg = regEnv.getOrElse(aid, allocate(aid))

    def wrapVC(vc: VC): VC = vc.fold(v => V(getOrAllocate(v)), _ => vc)

    for ((_ -> Return(ji, value, input)) <- f.body.jumps) {
      newChart.jumps(ji) = Return(ji, allocate(value), input)
    }

    for (block <- f.body.blocks.values.toSeq.reverseIterator) {
      val newLines = block.lines.reverseIterator.map {
        case Line(dest, inst) =>
          val newDest = regEnv(dest)
          Line(newDest, inst.mapAID(getOrAllocate))
      }.toList.reverse
      newChart.blocks(block.i) = Block(block.i, newLines, block.input, block.output)

      val ji1 = block.input
      val newJump = f.body.jumps(ji1) match {
        case j1: StartFun => Some(j1)
        case _: Return => ???
        case Condition(_, op, left, right, input, tru, fls) =>
          if (newChart.blocks.contains(tru) && newChart.blocks.contains(fls)) {
            Some(Condition(ji1, op, getOrAllocate(left), wrapVC(right), input, tru, fls))
          } else {
            None // newJumpの追加はtruとflsの両方が処理された後に行う
          }
        case Merge(_, inputs, outputID, output) =>
          val newInputs = inputs.map { case (aid, bi) => (allocate(aid), bi) }
          Some(Merge(ji1, newInputs, regEnv(outputID), output))
      }
      newJump.foreach(newChart.jumps(ji1) = _)
    }

    FDef(f.name, f.args.map(regEnv), newChart, f.typ)
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
