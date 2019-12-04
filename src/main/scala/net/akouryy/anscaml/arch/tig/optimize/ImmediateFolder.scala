package net.akouryy.anscaml
package arch.tig
package optimize

import asm._
import base._

import scala.collection.mutable

class ImmediateFolder(prog: Program) {
  def apply(): Boolean = {
    constRegEnv.clear()
    immEnv.clear()
    changed = false

    for (FDef(_, _, body, _) <- prog.functions) {
      body.blocks.valuesIterator.foreach(optBlock(body))
      body.jumps.valuesIterator.foreach(optJump(body))
    }

    changed
  }

  private[this] val constRegEnv = mutable.Map[AVar, AReg]()
  private[this] val immEnv = mutable.Map[AVar, Int]()
  private[this] var changed = false

  private[this] def wrapAID(aid: AID): AID = aid match {
    case r: AReg => r
    case v: AVar => constRegEnv.getOrElse(v, v)
  }

  private[this] def wrapVC(vc: VC): VC = vc match {
    case _: C => vc
    case V(r: AReg) => V(r)
    case V(v: AVar) => immEnv.get(v).foldX(C, V(v))
  }

  private[this] def optBlock(c: Chart)(b: Block): Unit = {
    var blockChanged = false

    val ls = b.lines.map { l =>
      val newInst = l.inst match {
        case Mv(v: AVar) =>
          l.dest match {
            case _: AReg => // pass
            case dest: AVar =>
              constRegEnv ++= constRegEnv.get(v).map(dest -> _)
              immEnv ++= immEnv.get(v).map(dest -> _)
          }
          None
        case Mv(r: AReg) =>
          l.dest match {
            case _: AReg => // pass
            case dest: AVar =>
              AReg.toConstants.get(r) match {
                case Some(i) =>
                  constRegEnv(dest) = r
                  immEnv(dest) = i
                case None => // pass
              }
          }
          None
        case Mvi(i) =>
          l.dest match {
            case _: AReg => // pass
            case dest: AVar =>
              immEnv(dest) = i
              AReg.fromConstants.get(i) match {
                case Some(r) => constRegEnv(dest) = r
                case None => // pass
              }
          }
          None
        case Fmvi(f) =>
          l.dest match {
            case _: AReg => // pass
            case dest: AVar =>
              val i = java.lang.Float.floatToRawIntBits(f)
              immEnv(dest) = i
              AReg.fromConstants.get(i) match {
                case Some(r) => constRegEnv(dest) = r
                case None => // pass
              }
          }
          None
        case NewArray(len, elem) => Some(NewArray(wrapVC(len), wrapAID(elem)))
        case Store(addr, index, value) => Some(Store(wrapAID(addr), wrapVC(index), wrapAID(value)))
        case Load(addr, index) => Some(Load(wrapAID(addr), wrapVC(index)))
        case UnOpTree(op, value) => Some(UnOpTree(op, wrapAID(value)))
        case BinOpVCTree(op, left, right) => Some(BinOpVCTree(op, wrapAID(left), wrapVC(right)))
        case BinOpVTree(op, left, right) => Some(BinOpVTree(op, wrapAID(left), wrapAID(right)))
        case Nop | Read => None
        case Write(value) => Some(Write(wrapAID(value)))
        case CallDir(fn, args) => Some(CallDir(fn, args.map(wrapAID)))
        case Save(_, _) | Restore(_) => ???
      }
      newInst match {
        case Some(i) if i != l.inst =>
          blockChanged = true
          l.copy(inst = i)
        case _ => l
      }
    }

    if (blockChanged) {
      changed = true
      c.blocks(b.i) = b.copy(lines = ls)
    }
  }

  private[this] def optJump(c: Chart)(j: Jump): Unit = {
    val newJ = j match {
      case StartFun(_, _) => j
      case Return(i, value, input) => Return(i, wrapAID(value), input)
      case Merge(i, inputs, outputID, output) =>
        Merge(i, inputs.map { case (aid, index) => (wrapAID(aid), index) }, outputID, output)
      case Condition(i, op, left, right, input, trueOutput, falseOutput) =>
        Condition(i, op, wrapAID(left), wrapVC(right), input, trueOutput, falseOutput)
    }

    if (newJ != j) {
      changed = true
      c.jumps(j.i) = newJ
    }
  }
}
