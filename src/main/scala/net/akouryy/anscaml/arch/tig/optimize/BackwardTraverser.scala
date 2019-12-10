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
  def apply(prog: Program): (Boolean, Map[BlockIndex, Set[XVar]]) = {
    changed = false
    useSets.clear()

    for (f <- prog.functions) {
      f.body.blocks.values.toSeq.reverseIterator.foreach(traverseBlock(f.body)) // TODO: reverse
      val free = useSets(f.body.blocks.firstKey) -- f.args.flatMap(_.asXVar)
      if (free.nonEmpty) {
        println(s"[Tig BackwardTraverser] free variables $free found.")
      }
    }

    (changed, useSets.toMap)
  }

  private[this] var changed = false
  private[this] val useSets = mutable.Map[BlockIndex, Set[XVar]]()

  /**
    * @param keep 副作用がなくてもブロックが保持されるかどうか。trueなら副作用がない命令でもuseが更新される
    * @return instが(ヒープ拡張以外の)副作用を持つかどうか
    */
  private[this] def traverseInst(keep: Boolean, use: mutable.Set[XVar], inst: Instruction)
  : Boolean = {
    inst match {
      case Mv(value) =>
        if (keep) {
          use ++= value.asXVar
        }
        false
      case _: Mvi | Nop => false
      case NewArray(len, elem) =>
        if (keep) {
          use ++= len.asV.flatMap(_.asXVar)
          use ++= elem.asXVar
        }
        false
      case Store(addr, index, value) =>
        use ++= addr.asXVar
        use ++= index.asV.flatMap(_.asXVar)
        use ++= value.asXVar
        true
      case Load(addr, index) =>
        if (keep) {
          use ++= addr.asXVar
          use ++= index.asV.flatMap(_.asXVar)
        }
        false
      case UnOpTree(_, value) =>
        if (keep) {
          use ++= value.asXVar
        }
        false
      case BinOpVCTree(_, left, right) =>
        if (keep) {
          use ++= left.asXVar
          use ++= right.asV.flatMap(_.asXVar)
        }
        false
      case BinOpVTree(_, left, right) =>
        if (keep) {
          use ++= left.asXVar
          use ++= right.asXVar
        }
        false
      case Read => true
      case Write(value) =>
        use ++= value.asXVar
        true
      case CallDir(_, args, None) =>
        use ++= args.flatMap(_.asXVar)
        true
      case inst => ????(inst)
    }
  }

  private[this] def traverseBlock(c: Chart)(b: Block): Unit = {
    val use = c.jumps(b.output) match {
      case j: StartFun => ????(j)
      case Return(_, _, value, _) => value.asXVar.to(mutable.Set)
      case Branch(_, _, Branch.Cond(_, left, right), _, tru, fls) =>
        val u = useSets(tru).to(mutable.Set)
        u ++= useSets(fls)
        u ++= left.asXVar
        u ++= right.asVXVar
        u
      case Merge(cm, i, inputs, outputID, output) =>
        val u = useSets(output).to(mutable.Set)
        if (outputID.asXVar.exists(!u.contains(_)) || outputID == XReg.DUMMY) {
          // outputID is not used
          c.jumps(i) = Merge(
            cm,
            i,
            inputs.map { case (_, index) => (XReg.DUMMY, index) },
            XReg.DUMMY,
            output
          )
        } else {
          // outputID is provably used
          u --= outputID.asXVar
          u ++= inputs.find(_._2 == b.i).get._1.asXVar
        }
        u
    }

    var isBlockChanging = false

    val ls = b.lines.reverseIterator.flatMap {
      case l @ Line(_, _: XReg, inst) =>
        traverseInst(keep = true, use, inst)
        Some(l)
      case l @ Line(cm, dest: XVar, inst) =>
        val keep = use.contains(dest)
        use -= dest

        val side = traverseInst(keep, use, inst)

        isBlockChanging ||= !keep && !side
        if (keep) {
          Some(l)
        } else if (side) {
          Some(Line(cm, XReg.DUMMY, inst))
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
