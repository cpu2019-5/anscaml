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
      immEnv.clear()
      otherEnv.clear()

      body.blocks.valuesIterator.foreach(optBlock(body))
      body.jumps.valuesIterator.foreach(optJump(body))
    }

    changed
  }

  private[this] object Fixed {
    def unapply(xid: XID): Option[Word] = xid.fold(immEnv.get, XReg.toConstants.get)

    def unapply(vc: VC): Option[Word] = vc.fold(unapply, Some(_))
  }

  private[this] object FixedRegByWord {
    def unapply(w: Word): Option[XReg] = XReg.fromConstants.get(w)
  }

  private[this] object FixedReg {
    def unapply(xid: XID): Option[XReg] = Fixed.unapply(xid).flatMap(FixedRegByWord.unapply)
  }

  private[this] object BoundTo {
    def unapply(xv: XVar): Option[Instruction] = otherEnv.get(xv)
  }

  private[this] val immEnv = mutable.Map[XVar, Word]()

  private[this] val otherEnv = mutable.Map[XVar, Instruction]()
  private[this] var changed = false

  private[this] def xidToConst(xid: XID): Option[Word] = xid.fold(immEnv.get, XReg.toConstants.get)

  private[this] def wrapXID(xid: XID): XID = xid match {
    case FixedReg(xr) => xr
    case _ => xid
  }

  private[this] def wrapVC(vc: VC): VC = vc match {
    case _: C => vc
    case V(r: XReg) => V(r)
    case V(Fixed(c)) => C(c)
    case V(x) => V(x)
  }

  private[this] def addImm(dest: XID, imm: Word) = dest match {
    case dest: XVar => immEnv(dest) = imm
    case _: XReg => // pass
  }

  private[this] def optBlock(c: Chart)(b: Block): Unit = {
    var blockChanged = false

    val ls = b.lines.flatMap { line =>
      val inst = line.inst
      var newPrecedingLines = List[Line]()
      val newInst: Instruction = inst match {
        case Mv(Fixed(w)) =>
          addImm(line.dest, w)
          inst
        case Mv(_) => inst

        case Mvi(w @ FixedRegByWord(r)) =>
          addImm(line.dest, w)
          Mv(r)
        case Mvi(w) =>
          addImm(line.dest, w)
          inst

        case NewArray(len, elem) => NewArray(wrapVC(len), wrapXID(elem))

        case Store(Fixed(a), C(index), value) =>
          Store(XReg.ZERO, C.int(a.int + index.int), wrapXID(value))
        case Store(addr, C(index), value) =>
          Store(addr, C(index), wrapXID(value))

        case Load(Fixed(a), Fixed(i)) =>
          Load(XReg.ZERO, C.int(a.int + i.int))
        case Load(Fixed(a), index) =>
          Load(wrapXID(index.asV.get), C(a))
        case Load(addr, index) =>
          Load(addr, wrapVC(index))

        case UnOpTree(op, value) => UnOpTree(op, wrapXID(value))

        case BinOpVCTree(op, Fixed(l), Fixed(r)) =>
          val imm = op.fn(l, r)
          addImm(line.dest, imm)
          Mvi(imm)
        case BinOpVCTree(op, Fixed(l), right) if op.isCommutative =>
          BinOpVCTree(op, right.asV.get, C(l))
        case BinOpVCTree(op, left, right) =>
          BinOpVCTree(op, left, wrapVC(right))

        case BinOpVTree(op, Fixed(l), Fixed(r)) =>
          val imm = op.fn(l, r)
          addImm(line.dest, imm)
          Mvi(imm)
        case BinOpVTree(Sub, left, Fixed(r)) if emit.FinalArg.SImm.dom contains -r.int =>
          BinOpVCTree(Add, left, C.int(-r.int))
        case BinOpVTree(Fdiv, Fixed(Word.WithFloat(1.0F)), right) =>
          UnOpTree(FInv, right)
        case BinOpVTree(Fdiv, Fixed(Word.WithFloat(-1.0F)), right) =>
          val inv = XVar.generate(right.idStr + ID.Special.ASM_F_INV)
          newPrecedingLines = List(Line(NC, inv, UnOpTree(FInv, right)))
          BinOpVTree(FnegCond, inv, XReg.C_MINUS_ONE)
        case BinOpVTree(FnegCond, left @ BoundTo(BinOpVTree(Fadd, al, ar)), right) if left == right
        => /* 絶対値 */
          BinOpVTree(FaddAbs, al, ar)
        case BinOpVTree(op, left, right) => BinOpVTree(op, wrapXID(left), wrapXID(right))

        case Nop | Read => inst
        case Write(value) => Write(wrapXID(value))
        case CallDir(fn, args, None) => CallDir(fn, args.map(wrapXID), None)
        case _ => !!!!(inst)
      }

      line.dest.asXVar.foreach {
        otherEnv(_) = newInst match {
          case Mv(x: XVar) => otherEnv.getOrElse(x, newInst)
          case _ => newInst
        }
      }
      if (newInst != line.inst || newPrecedingLines.nonEmpty) {
        blockChanged = true
        newPrecedingLines :+ line.copy(inst = newInst)
      } else {
        List(line)
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
      case Branch(cm, i, cond, input, tru, fls) =>
        import Branch._
        val (preserveTruAndFls, newComment, newCond) = cond match {
          case Cond(op, Fixed(l), Fixed(r)) =>
            // 定数標準形(常にnewTruに遷移する; JumpFolder参照)
            (op.fn(l, r), NC, CondVC(Eq, XReg.ZERO, V(XReg.ZERO)))
          case CondVC(Le, BoundTo(BinOpVTree(Sub, XReg.C_MINUS_ONE, orig)), Fixed(Word(-1))) =>
            // 否定を除去してジャンプ先入れ替え
            (false, CM("[IF] if-not"), CondVC(Le, orig, C.int(-1)))
          case CondV(FLe, Fixed(Word(0)), right) => // 符号判定はより定義域の広いLeに統一する
            (true, NC, CondVC(Le, XReg.ZERO, V(right)))
          case CondV(FLe, left, Fixed(Word(0))) =>
            (true, NC, CondVC(Le, left, V(XReg.ZERO)))
          case _ =>
            (true, NC, cond.mapLR(wrapXID)(wrapVC, wrapXID))
        }
        Branch(
          cm + newComment, i, newCond, input,
          if (preserveTruAndFls) tru else fls, if (preserveTruAndFls) fls else tru,
        )
    }

    if (newJ != j) {
      changed = true
      c.jumps(j.i) = newJ
    }
  }
}
