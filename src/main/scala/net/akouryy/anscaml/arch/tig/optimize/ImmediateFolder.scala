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
      selectEnv.clear()

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
    def unapply(xid: XID): Option[XReg] = xid match {
      case xv: XVar => immEnv.get(xv).flatMap(XReg.fromConstants.get)
      case xr: XReg => Option.when(XReg.toConstants contains xr)(xr)
    }
  }

  private[this] object Selection {
    def unapply(xv: XVar): Option[(Branch.Cond, XID, XID)] =
      selectEnv.get(xv).flatMap(Select.unapply)
  }

  private[this] object LoadedTo {
    def unapply(l: Load): Option[XVar] = loadEnv.get(l.originalIndex).flatMap(_.get(l))
  }

  private[this] object BoundTo {
    def unapply(xv: XVar): Option[Instruction] = otherEnv.get(xv)
  }

  private[this] val immEnv = mutable.Map[XVar, Word]()

  private[this] val otherEnv = mutable.Map[XVar, Instruction]()
  private[this] val selectEnv = mutable.Map[XVar, Select]()
  private[this] val loadEnv =
    mutable.Map[MemoryIndex, mutable.Map[Load, XVar]]()
  private[this] var changed = false

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
    loadEnv.clear()

    val ls = b.lines.flatMap { line =>
      val inst = line.inst
      var newPrecedingLines = List[Line]()
      val newInst: Instruction = inst match {
        case Mv(Fixed(w)) =>
          addImm(line.dest, w)
          inst
        case Mv(Selection(sel)) =>
          for (v <- line.dest.asXVar) selectEnv(v) = Select.tupled(sel)
          inst
        case Mv(_) => inst

        case Mvi(w @ FixedRegByWord(r)) =>
          addImm(line.dest, w)
          Mv(r)
        case Mvi(w) =>
          addImm(line.dest, w)
          inst

        case NewArray(len, elem) => NewArray(wrapVC(len), wrapXID(elem))

        case Store(Fixed(a), C(index), value, orig) =>
          Store(XReg.ZERO, C.int(a.int + index.int), wrapXID(value), orig)
        case Store(addr, C(index), value, orig) =>
          Store(addr, C(index), wrapXID(value), orig)

        case LoadedTo(v) =>
          Mv(v)
        case Load(Fixed(a), Fixed(i), orig) =>
          Load(XReg.ZERO, C.int(a.int + i.int), orig)
        case Load(Fixed(a), index, orig) =>
          Load(wrapXID(index.asV.get), C(a), orig)
        case Load(addr, index, orig) =>
          Load(addr, wrapVC(index), orig)

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
        case BinOpVTree(Sub, Fixed(Word(-1)), Selection(cond, FixedReg(tru), FixedReg(fls))) =>
          val sel = Select(cond, fls, tru)
          for (v <- line.dest.asXVar) selectEnv(v) = sel
          sel
        case BinOpVTree(Fdiv, Fixed(Word.WithFloat(1.0F)), right) =>
          UnOpTree(FInv, right)
        case BinOpVTree(Fdiv, Fixed(Word.WithFloat(-1.0F)), right) =>
          val inv = XVar.generate(right.idStr + ID.Special.ASM_F_INV)
          newPrecedingLines = List(Line(NC, inv, UnOpTree(FInv, right)))
          BinOpVTree(FnegCond, inv, XReg.C_MINUS_ONE)
        case BinOpVTree(FnegCond, left @ BoundTo(BinOpVTree(Fadd, al, ar)), right)
          if left == right => // 絶対値
          BinOpVTree(FaddAbs, al, ar)
        case BinOpVTree(FnegCond, left,
        Selection(Branch.CondVC(Le, Fixed(Word(0)), V(orig)), Fixed(t), Fixed(f))) =>
          @inline def withOrig = BinOpVTree(FnegCond, left, orig)

          if (t.int >= 0 && f.int < 0) { // rightとorigの真偽は等しい
            withOrig
          } else if (t.int < 0 && f.int >= 0) { // rightとorigの真偽は逆
            val withOrigResult = XVar.generate(orig.idStr + ID.Special.ASM_NOT)
            newPrecedingLines = List(Line(NC, withOrigResult, withOrig))
            BinOpVTree(FnegCond, withOrigResult, XReg.C_MINUS_ONE)
          } else ???
        case BinOpVTree(op, left, right) => BinOpVTree(op, wrapXID(left), wrapXID(right))

        case Select(cond, tru, fls) =>
          val (preserveTruAndFls, _, newCond) = optCond(cond)
          val (newTru, newFls) = if (preserveTruAndFls) (tru, fls) else (fls, tru)
          Select(newCond.mapLR(wrapXID)(wrapVC, wrapXID), wrapXID(newTru), wrapXID(newFls))
        case Nop | Read => inst
        case Write(value) => Write(wrapXID(value))
        case CallDir(fn, args, None) =>
          loadEnv.clear()
          CallDir(fn, args.map(wrapXID), None)
        case _ => !!!!(inst)
      }


      newInst match {
        case Mv(x: XVar) =>
          line.dest.asXVar.foreach { v =>
            otherEnv(v) = otherEnv.getOrElse(x, newInst)
          }
        case newInst: Load =>
          line.dest.asXVar.foreach { v =>
            loadEnv.getOrElseUpdate(newInst.originalIndex, mutable.Map())(newInst) = v
          }
        case Store(_, _, _, orig) =>
          loadEnv.filterInPlace((mi, _) => mi !~ orig)
        case newInst: Select =>
          line.dest.asXVar.foreach { v =>
            selectEnv(v) = newInst
            otherEnv(v) = newInst
          }
        case _ =>
          line.dest.asXVar.foreach { v =>
            otherEnv(v) = newInst
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

  /**
    * @return (true if the meaning is preserved(<b>not</b> negated), comment, optimized condition).
    */
  private[this] def optCond(cond: Branch.Cond): (Boolean, Comment, Branch.Cond) = {
    import Branch._
    cond match {
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
      case Cond(op, Selection(cond2, Fixed(t), Fixed(f)), Fixed(right)) =>
        if (op.fn(t, right) && !op.fn(f, right)) {
          (true, NC, cond2)
        } else if (!op.fn(t, right) && op.fn(f, right)) {
          (false, NC, cond2)
        } else ????(cond)
      case Cond(_, _, V(FixedReg(r))) =>
        (true, NC, cond.mapLR(wrapXID)(_ => V(r), _ => r))
      case _ =>
        (true, NC, cond.mapLR(wrapXID)(wrapVC, wrapXID))

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
        val (preserveTruAndFls, newComment, newCond) = optCond(cond)
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
