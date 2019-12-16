package net.akouryy.anscaml
package arch.tig
package optimize

import asm._
import base._

import scala.collection.mutable

class ImmediateFolder(prog: Program) {
  def apply(): Boolean = {
    changed = false

    for (FDef(_, _, body, _, _) <- prog.functions) {
      constRegEnv.clear()
      immEnv.clear()
      otherEnv.clear()

      body.blocks.valuesIterator.foreach(optBlock(body))
      body.jumps.valuesIterator.foreach(optJump(body))
    }

    changed
  }

  private[this] val constRegEnv = mutable.Map[XVar, XReg]()
  private[this] val immEnv = mutable.Map[XVar, Word]()

  private[this] val otherEnv = mutable.Map[XVar, Instruction]()
  private[this] var changed = false

  private[this] def xidToConst(xid: XID): Option[Word] = xid.fold(immEnv.get, XReg.toConstants.get)

  private[this] def vcToConst(vc: VC): Option[Word] = vc.fold(xidToConst, Some(_))

  private[this] def wrapXID(xid: XID): XID = xid.fold(v => constRegEnv.getOrElse(v, v), identity)

  private[this] def wrapVC(vc: VC): VC = vc match {
    case _: C => vc
    case V(r: XReg) => V(r)
    case V(v: XVar) => immEnv.get(v).foldF(C, V(v))
  }

  private[this] def getOther(xid: XID) = xid.asXVar.flatMap(otherEnv.get)

  private[this] def addImm(dest: XID, imm: Word) = dest match {
    case dest: XVar =>
      immEnv(dest) = imm
      XReg.fromConstants.get(imm) match {
        case Some(r) => constRegEnv(dest) = r
        case None => // pass
      }
    case _: XReg => // pass
  }

  private[this] def optBlock(c: Chart)(b: Block): Unit = {
    var blockChanged = false

    val ls = b.lines.flatMap { line =>
      val newInst: Option[(List[Line], Instruction)] = line.inst match {
        case Mv(v: XVar) =>
          line.dest match {
            case _: XReg => // pass
            case dest: XVar =>
              constRegEnv ++= constRegEnv.get(v).map(dest -> _)
              immEnv ++= immEnv.get(v).map(dest -> _)
          }
          None
        case Mv(r: XReg) =>
          (line.dest, XReg.toConstants.get(r)) match {
            case (dest: XVar, Some(i)) =>
              constRegEnv(dest) = r
              immEnv(dest) = i
            case _ => // pass
          }
          None
        case Mvi(w) =>
          addImm(line.dest, w)
          XReg.fromConstants.get(w).map(r => Nil -> Mv(r))
        case NewArray(len, elem) => Some(Nil -> NewArray(wrapVC(len), wrapXID(elem)))
        case Store(addr, C(index), value) =>
          xidToConst(addr) match {
            case Some(a) =>
              Some(Nil -> Store(XReg.ZERO, C(Word.fromInt(a.int + index.int)), wrapXID(value)))
            case None =>
              Some(Nil -> Store(wrapXID(addr), C(index), wrapXID(value)))
          }
        case Load(addr, index) =>
          (xidToConst(addr), vcToConst(index)) match {
            case (Some(a), Some(i)) =>
              Some(Nil -> Load(XReg.ZERO, C.int(a.int + i.int)))
            case (Some(a), None) =>
              Some(Nil -> Load(wrapXID(index.asInstanceOf[V].v), C(a)))
            case _ =>
              Some(Nil -> Load(wrapXID(addr), wrapVC(index)))
          }
        case UnOpTree(op, value) => Some(Nil -> UnOpTree(op, wrapXID(value)))
        case BinOpVCTree(op, left, right) =>
          (xidToConst(left), vcToConst(right)) match {
            case (Some(l), Some(r)) =>
              val imm = op.fn(l, r)
              addImm(line.dest, imm)
              Some(Nil -> Mvi(imm))
            case (Some(l), None) if op.isCommutative =>
              Some(Nil -> BinOpVCTree(op, right.asV.get, C(l)))
            case _ =>
              Some(Nil -> BinOpVCTree(op, wrapXID(left), wrapVC(right)))
          }
        case BinOpVTree(op, left, right) =>
          (op, xidToConst(left), xidToConst(right)) match {
            case (_, Some(l), Some(r)) =>
              val imm = op.fn(l, r)
              addImm(line.dest, imm)
              Some(Nil -> Mvi(imm))
            case (Sub, _, Some(r)) if emit.FinalArg.SImm.dom contains -r.int =>
              Some(Nil -> BinOpVCTree(Add, wrapXID(left), C.int(-r.int)))
            case (Fdiv(FFOrd), Some(floatOne), _) if floatOne.float == 1.0F =>
              Some(Nil -> UnOpTree(FInv(FFOrd), right))
            case (Fdiv(FFOrd), Some(floatMinusOne), _) if floatMinusOne.float == -1.0F =>
              Some(Nil -> UnOpTree(FInv(FFNeg), right))
            case _ =>
              (op, getOther(left), right) match {
                case (FnegCond, Some(BinOpVTree(Fadd(FFOrd), al, ar)), _)
                  if left == right /* 絶対値 */ =>
                  Some(Nil -> BinOpVTree(Fadd(FFAbs), al, ar))
                case (FnegCond, Some(BinOpVTree(Fadd(FFOrd), al, ar)), XReg.C_MINUS_ONE) =>
                  Some(Nil -> BinOpVTree(Fadd(FFNeg), al, ar))
                case (FnegCond, Some(BinOpVTree(Fmul(FFOrd), al, ar)), XReg.C_MINUS_ONE) =>
                  Some(Nil -> BinOpVTree(Fmul(FFNeg), al, ar))
                case (FnegCond, Some(BinOpVTree(Fdiv(FFOrd), al, ar)), XReg.C_MINUS_ONE) =>
                  Some(Nil -> BinOpVTree(Fdiv(FFNeg), al, ar))
                case _ => Some(Nil -> BinOpVTree(op, wrapXID(left), wrapXID(right)))
              }
          }
        case Nop | Read => None
        case Write(value) => Some(Nil -> Write(wrapXID(value)))
        case CallDir(fn, args, None) => Some(Nil -> CallDir(fn, args.map(wrapXID), None))
        case inst => !!!!(inst)
      }

      line.dest.asXVar.foreach {
        otherEnv(_) = newInst.getOrElse(Nil -> line.inst)._2 match {
          case ins @ Mv(x) => getOther(x) getOrElse ins
          case ins => ins
        }
      }
      newInst match {
        case Some((lines, inst)) if inst != line.inst || lines.nonEmpty =>
          blockChanged = true
          lines :+ line.copy(inst = inst)
        case _ => List(line)
      }
    }

    if (blockChanged) {
      changed = true
      c.blocks(b.i) = b.copy(lines = ls)
    }
  }

  private[this] def optJump(c: Chart)(j: Jump): Unit = {
    val newJ = j match {
      case StartFun(_, _, _) => j
      case Return(cm, i, value, input) => Return(cm, i, wrapXID(value), input)
      case Merge(cm, i, inputs, outputID, output) =>
        Merge(
          cm, i,
          inputs.map(_.mapXID(wrapXID)),
          outputID,
          output
        )
      case Branch(cm, i, Branch.CondVC(op, left, right), input, tru, fls) =>
        (xidToConst(left), vcToConst(right)) match {
          case (Some(l), Some(r)) =>
            // 定数標準形(JumpFolder参照)
            val result = Word.fromInt(if (op.fn(l, r)) 0 else -1)
            Branch(cm, i, Branch.CondVC(Eq, XReg.ZERO, C(result)), input, tru, fls)
          case _ =>
            (op, getOther(left), right) match {
              case (Le, Some(BinOpVTree(Sub, XReg.C_MINUS_ONE, orig)), C(Word(-1))) =>
                // 否定を除去してジャンプ先入れ替え
                Branch(cm :+ "[IF] if-not", i, Branch.CondVC(Le, orig, C.int(-1)), input, fls, tru)
              case _ =>
                Branch(cm, i, Branch.CondVC(op, wrapXID(left), wrapVC(right)), input, tru, fls)
            }
        }
      case Branch(cm, i, Branch.CondV(op, left, right), input, tru, fls) =>
        (op, xidToConst(left), xidToConst(right)) match {
          case (_, Some(l), Some(r)) =>
            // 定数標準形(JumpFolder参照)
            val result = Word.fromInt(if (op.fn(l, r)) 0 else -1)
            Branch(cm, i, Branch.CondVC(Eq, XReg.ZERO, C(result)), input, tru, fls)
          case (FLe, Some(Word(0)), _) => // 符号判定はより定義域の広いLeに統一する
            Branch(cm, i, Branch.CondVC(Le, XReg.ZERO, V(right)), input, tru, fls)
          case (FLe, _, Some(Word(0))) =>
            Branch(cm, i, Branch.CondVC(Le, left, V(XReg.ZERO)), input, tru, fls)
          case _ => Branch(cm, i, Branch.CondV(op, wrapXID(left), wrapXID(right)), input, tru, fls)
        }
    }

    if (newJ != j) {
      changed = true
      c.jumps(j.i) = newJ
    }
  }
}
