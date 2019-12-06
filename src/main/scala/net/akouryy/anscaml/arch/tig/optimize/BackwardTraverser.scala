package net.akouryy.anscaml
package arch.tig
package optimize

import asm._
import base._

import scala.collection.mutable

/**
  * グラフ構造を変えない最適化
  */
class BackwardTraverser {
  def apply(prog: Program): (Boolean, Map[BlockIndex, Set[AVar]]) = {
    changed = false
    useSets.clear()

    for (f <- prog.functions) {
      f.body.blocks.values.toSeq.reverseIterator.foreach(traverseBlock(f.body)) // TODO: reverse
      val free = useSets(f.body.blocks.firstKey) -- f.args.flatMap(_.aVarOpt)
      if (free.nonEmpty) {
        println(s"[Tig BackwardTraverser] free variables $free found.")
      }
    }

    (changed, useSets.toMap)
  }

  private[this] var changed = false
  private[this] val useSets = mutable.Map[BlockIndex, Set[AVar]]()

  /**
    * @param keep 副作用がなくてもブロックが保持されるかどうか。trueなら副作用がない命令でもuseが更新される
    * @return instが(ヒープ拡張以外の)副作用を持つかどうか
    */
  private[this] def traverseInst(keep: Boolean, use: mutable.Set[AVar], inst: Instruction)
  : Boolean = {
    inst match {
      case Mv(value) =>
        if (keep) {
          use ++= value.aVarOpt
        }
        false
      case _: Mvi | _: Fmvi | Nop => false
      case NewArray(len, elem) =>
        if (keep) {
          use ++= len.vOpt.flatMap(_.aVarOpt)
          use ++= elem.aVarOpt
        }
        false
      case Store(addr, index, value) =>
        use ++= addr.aVarOpt
        use ++= index.vOpt.flatMap(_.aVarOpt)
        use ++= value.aVarOpt
        true
      case Load(addr, index) =>
        if (keep) {
          use ++= addr.aVarOpt
          use ++= index.vOpt.flatMap(_.aVarOpt)
        }
        false
      case UnOpTree(_, value) =>
        if (keep) {
          use ++= value.aVarOpt
        }
        false
      case BinOpVCTree(_, left, right) =>
        if (keep) {
          use ++= left.aVarOpt
          use ++= right.vOpt.flatMap(_.aVarOpt)
        }
        false
      case BinOpVTree(_, left, right) =>
        if (keep) {
          use ++= left.aVarOpt
          use ++= right.aVarOpt
        }
        false
      case Read => true
      case Write(value) =>
        use ++= value.aVarOpt
        true
      case CallDir(_, args) =>
        use ++= args.flatMap(_.aVarOpt)
        true
      case _: Save | _: Restore => ???
    }
  }

  private[this] def traverseBlock(c: Chart)(b: Block): Unit = {
    val use = c.jumps(b.output) match {
      case _: StartFun => ???
      case Return(_, value, _) => value.aVarOpt.to(mutable.Set)
      case Condition(_, _, left, right, _, tru, fls) =>
        val u = useSets(tru).to(mutable.Set)
        u ++= useSets(fls)
        u ++= left.aVarOpt
        u ++= right.vOpt.flatMap(_.aVarOpt)
        u
      case Merge(i, inputs, outputID, output) =>
        val u = useSets(output).to(mutable.Set)
        if (outputID.aVarOpt.exists(!u.contains(_)) || outputID == AReg.REG_DUMMY) {
          // outputID is not used
          c.jumps(i) = Merge(
            i,
            inputs.map { case (_, index) => (AReg.REG_DUMMY, index) },
            AReg.REG_DUMMY,
            output
          )
        } else {
          // outputID is provably used
          u --= outputID.aVarOpt
          u ++= inputs.find(_._2 == b.i).get._1.aVarOpt
        }
        u
    }

    var isBlockChanging = false

    val ls = b.lines.reverseIterator.flatMap {
      case l @ Line(_: AReg, inst) =>
        traverseInst(keep = true, use, inst)
        Some(l)
      case l @ Line(dest: AVar, inst) =>
        val keep = use.contains(dest)
        use -= dest

        val side = traverseInst(keep, use, inst)

        isBlockChanging ||= !keep && !side
        if (keep) {
          Some(l)
        } else if (side) {
          Some(Line(AReg.REG_DUMMY, inst))
        } else {
          None
        }
    }.toList.reverse

    if (isBlockChanging) {
      c.blocks(b.i) = b.copy(lines = ls)
    }

    changed ||= isBlockChanging
    useSets(b.i) = use.toSet
  }
}
