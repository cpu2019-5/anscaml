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

  private[this] val constRegEnv = mutable.Map[XVar, XReg]()
  private[this] val immEnv = mutable.Map[XVar, Word]()
  private[this] var changed = false

  private[this] def xidToConst(xid: XID): Option[Word] = xid.fold(immEnv.get, XReg.toConstants.get)

  private[this] def vcToConst(vc: VC): Option[Word] = vc.fold(xidToConst, Some(_))

  private[this] def wrapXID(xid: XID): XID = xid.fold(v => constRegEnv.getOrElse(v, v), identity)

  private[this] def wrapVC(vc: VC): VC = vc match {
    case _: C => vc
    case V(r: XReg) => V(r)
    case V(v: XVar) => immEnv.get(v).foldF(C, V(v))
  }

  private[this] def optBlock(c: Chart)(b: Block): Unit = {
    var blockChanged = false

    val ls = b.lines.map { l =>
      val newInst = l.inst match {
        case Mv(v: XVar) =>
          l.dest match {
            case _: XReg => // pass
            case dest: XVar =>
              constRegEnv ++= constRegEnv.get(v).map(dest -> _)
              immEnv ++= immEnv.get(v).map(dest -> _)
          }
          None
        case Mv(r: XReg) =>
          l.dest match {
            case _: XReg => // pass
            case dest: XVar =>
              XReg.toConstants.get(r) match {
                case Some(i) =>
                  constRegEnv(dest) = r
                  immEnv(dest) = i
                case None => // pass
              }
          }
          None
        case Mvi(w) =>
          l.dest match {
            case _: XReg => // pass
            case dest: XVar =>
              immEnv(dest) = w
              XReg.fromConstants.get(w) match {
                case Some(r) => constRegEnv(dest) = r
                case None => // pass
              }
          }
          None
        case NewArray(len, elem) => Some(NewArray(wrapVC(len), wrapXID(elem)))
        case Store(addr, index, value) =>
          (xidToConst(addr), vcToConst(index)) match {
            case (Some(a), Some(i)) =>
              Some(Store(XReg.REG_ZERO, C(Word.fromInt(a.int + i.int)), wrapXID(value)))
            case (Some(a), None) =>
              Some(Store(wrapXID(index.asInstanceOf[V].v), C(a), wrapXID(value)))
            case _ =>
              Some(Store(wrapXID(addr), wrapVC(index), wrapXID(value)))
          }
        case Load(addr, index) =>
          (xidToConst(addr), vcToConst(index)) match {
            case (Some(a), Some(i)) =>
              Some(Load(XReg.REG_ZERO, C(Word.fromInt(a.int + i.int))))
            case (Some(a), None) =>
              Some(Load(wrapXID(index.asInstanceOf[V].v), C(a)))
            case _ =>
              Some(Load(wrapXID(addr), wrapVC(index)))
          }
        case UnOpTree(op, value) => Some(UnOpTree(op, wrapXID(value)))
        case BinOpVCTree(op, left, right) =>
          (xidToConst(left), vcToConst(right)) match {
            case (Some(l), Some(r)) =>
              Some(Mvi(op.fn(l, r)))
            case _ =>
              Some(BinOpVCTree(op, wrapXID(left), wrapVC(right)))
          }
        case BinOpVTree(op, left, right) => Some(BinOpVTree(op, wrapXID(left), wrapXID(right)))
        case Nop | Read => None
        case Write(value) => Some(Write(wrapXID(value)))
        case CallDir(fn, args) => Some(CallDir(fn, args.map(wrapXID)))
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
      case Return(i, value, input) => Return(i, wrapXID(value), input)
      case Merge(i, inputs, outputID, output) =>
        Merge(i, inputs.map { case (xid, index) => (wrapXID(xid), index) }, outputID, output)
      case Condition(i, op, left, right, input, trueOutput, falseOutput) =>
        (xidToConst(left), vcToConst(right)) match {
          case (Some(l), Some(r)) =>
            // 定数標準形(JumpFolder参照)
            val result = Word.fromInt(if (op.fn(l, r)) 1 else 0)
            Condition(i, Eq, XReg.REG_ZERO, C(result), input, trueOutput, falseOutput)
          case _ =>
            Condition(i, op, wrapXID(left), wrapVC(right), input, trueOutput, falseOutput)
        }
    }

    if (newJ != j) {
      changed = true
      c.jumps(j.i) = newJ
    }
  }
}
