package net.akouryy.anscaml
package arch.tig
package emit

import java.io.PrintWriter

import asm._
import base._
import emit.{FinalInst => FInst}
import FinalArg.{Label => FLabel, LAbs, LRel, SImm, TImm, UImm}

import scala.collection.mutable

class Emitter(program: Program) {

  import Emitter._

  private[this] case class MaxStackSize(n: Int)

  private[this] val StackMetaDataSize = 1 // リンクレジスタ退避用
  private[this] val LinkRegOffset = -1

  private[this] val fixedFLines = mutable.ListBuffer[FinalLine]()
  private[this] var currentFun: FDef = _
  private[this] val emittedBlocks = mutable.Set[BlockIndex]()
  private[this] val currentStack = mutable.ArrayBuffer[ID]()
  private[this] val currentStackInv = mutable.Map[ID, Int]()
  private[this] var currentStackMaxSize: MaxStackSize = _
  private[this] val currentFLines =
    mutable.ListBuffer[MaxStackSize => Seq[FinalLine]]()

  private[this] def draft(fLine: FinalLine): Unit = currentFLines += (_ => Seq(fLine))

  private[this] def draftCommand(comment: Comment, inst: FInst, args: FinalArg*): Unit =
    draft(FinalCommand(comment, inst, args: _*))

  private[this] def draftLabel(label: String): Unit =
    currentFLines += (_ => Seq(FinalLabel(label, "")))

  private[this] def FReg(xReg: XReg) = FinalArg.Reg(xReg.toString)

  private[this] def blockLabel(bi: BlockIndex) =
    s"${currentFun.name}.${bi.indexString}"

  private[this] def draftMv(comment: Comment, dest: XReg, src: XReg) = {
    if (dest != src) {
      draftCommand(comment, FInst.band, FReg(dest), FReg(src), FReg(src))
    }
  }

  private[this] def draftMvi(cm: Comment, dest: XReg, word: Word) = {
    val value = word.int
    val higher = value >>> 16
    val lower = value & (1 << 16) - 1
    val cm2 = cm :+ s"[E] mvi $word"
    val higherLHS =
      if (lower != 0) {
        draftCommand(cm2, FInst.bandi, FReg(dest), FReg(XReg.C_MINUS_ONE), UImm(lower))
        dest
      } else {
        XReg.ZERO
      }
    if (higher != 0 || lower == 0) { // lower==0のときも入れないとdestへの代入が一切行われないままになる
      draftCommand(cm2, FInst.orhi, FReg(dest), FReg(higherLHS), UImm(higher))
    }
  }

  private[this] def draftRevertStack(): Unit =
    if (!currentFun.info.isLeaf) {
      currentFLines += { case MaxStackSize(sz) =>
        if (sz == 0) Nil
        else List(
          FinalCommand(CM("[E] revert stack"),
            FInst.addi, FReg(XReg.STACK), FReg(XReg.STACK), SImm(+sz))
        )
      }
      draftCommand(CM("[E] restore LR"),
        FInst.load, FReg(XReg.LINK), FReg(XReg.STACK), SImm(LinkRegOffset))
    }

  private[this] def emitLine(l: Line, isTail: Boolean): Unit = {
    val cm = l.comment
    val dest = l.dest.asXReg.get
    val toDummy = l.dest == XReg.DUMMY
    l.inst match {
      case Mv(id: XReg) if !toDummy => draftMv(cm, dest, id)
      case Mvi(value) if !toDummy => draftMvi(cm, dest, value)
      case NewArray(V(len: XReg), elem: XReg) if !toDummy =>
        // do-whileループでtmpが0以上である間拡張を繰り返す
        // len=0のとき1回ループしてしまうが、未定義領域に1個書き込むだけなので許容
        val bodyLabel = ID.generate(ID(s"${currentFun.name}.${ID.Special.EMIT_ARRAY_BODY}")).str
        draftMv(CM(s"[E] NewArray"), XReg.LAST_TMP, len)
        draftLabel(bodyLabel)
        draftCommand(NC, FInst.store, FReg(XReg.HEAP), SImm(0), FReg(elem))
        draftCommand(NC, FInst.addi, FReg(XReg.HEAP), FReg(XReg.HEAP), SImm(1))
        draftCommand(NC, FInst.addi, FReg(XReg.LAST_TMP), FReg(XReg.LAST_TMP), SImm(-1))
        draftCommand(NC, FInst.jgt, FReg(XReg.LAST_TMP), FReg(XReg.ZERO), FLabel(LRel, bodyLabel))
        draftCommand(cm, FInst.sub, FReg(dest), FReg(XReg.HEAP), FReg(len))
      case NewArray(C(len), elem: XReg) if !toDummy =>
        for (i <- 0 until len.int) {
          draftCommand(NC, FInst.store, FReg(XReg.HEAP), SImm(i), FReg(elem))
        }
        draftMv(cm, dest, XReg.HEAP)
        draftCommand(NC, FInst.addi, FReg(XReg.HEAP), FReg(XReg.HEAP), SImm(len.int))
      case Store(addr: XReg, index, value: XReg) if toDummy =>
        draftCommand(cm, FInst.store, FReg(addr), SImm(index.c.int), FReg(value))
      case Load(addr: XReg, V(index: XReg)) if !toDummy =>
        draftCommand(cm, FInst.loadreg, FReg(dest), FReg(addr), FReg(index))
      case Load(addr: XReg, C(index)) if !toDummy =>
        draftCommand(cm, FInst.load, FReg(dest), FReg(addr), SImm(index.int))
      case UnOpTree(op, value: XReg) if !toDummy =>
        draftCommand(cm, FInst.fromUnOp(op), FReg(dest), FReg(value))
      case BinOpVCTree(op, left: XReg, right @ (V(_: XReg) | _: C)) if !toDummy =>
        draftCommand(cm,
          right.fold(_ => FInst.vFromBinOpVC(op), _ => FInst.cFromBinOpVC(op)),
          FReg(dest),
          FReg(left),
          right.fold(
            v => FReg(v.asXReg.get),
            c =>
              if (op == asm.Sha) TImm(c.int)
              else if (Seq(asm.Band, asm.Bor) contains op) UImm(c.int)
              else SImm(c.int)
          ),
        )
      case BinOpVTree(op, left: XReg, right: XReg) if !toDummy =>
        draftCommand(cm, FInst.fromBinOpV(op), FReg(dest), FReg(left), FReg(right))
      case Nop if toDummy => // nop
      case Read => draftCommand(cm, FInst.read, FReg(if (toDummy) XReg.ZERO else dest))
      case Write(value: XReg) if toDummy => draftCommand(cm, FInst.write, FReg(value))
      case CallDir(ID.Special.ASM_EXIT_FUN, Nil, Some(_)) if toDummy => draftCommand(cm, FInst.exit)
      case CallDir(fn, args, Some(_)) if isTail /* destはdummyでもそうでなくてもよい */ =>
        if (!Seq(XReg.DUMMY, XReg.RETURN).contains(dest)) !!!!(l)

        val argMoves = moveSimultaneously(
          args.zipWithIndex.map {
            case (a, i) => Move(a.asXReg.get, XReg.NORMAL_REGS(i))
          }
        )

        draftRevertStack()
        for (Move(s, d) <- argMoves) draftMv(CM("[E] move arg"), d, s)
        draftCommand(cm :+ "[E] tail call", FInst.j, FLabel(LAbs, fn))

      case CallDir(fn, args, Some(saves)) /* destはdummyでもそうでなくてもよい */ =>
        currentStack.clear()
        currentStackInv.clear()

        val savedKeyPositions = saves.keys.flatMap { k =>
          currentStackInv.get(k).map(k -> _)
        }.to(mutable.Map)
        val savedKeysByPosition = savedKeyPositions.map(_.swap)

        val newSaves = mutable.ListBuffer[(Int, ID)]()
        for {
          (key, reg) <- saves
          if !savedKeyPositions.contains(key)
        } {
          val i = (0 until saves.size).find(i => !savedKeysByPosition.contains(i)).get
          savedKeyPositions(key) = i
          savedKeysByPosition(i) = key
          newSaves += ((i, key))
          if (i < currentStack.size) {
            currentStack(i) = key
          } else {
            assert(i == currentStack.size)
            currentStack += key
          }
          currentStackInv(key) = i
        }

        currentStackMaxSize = MaxStackSize(
          currentStackMaxSize.n.max(StackMetaDataSize + currentStack.size)
        )

        val argMoves = moveSimultaneously(
          args.zipWithIndex.map {
            case (a, i) => Move(a.asXReg.get, XReg.NORMAL_REGS(i))
          }
        )

        for ((i, key) <- newSaves) {
          draftCommand(CM(s"[E] save local ${key.str}"), FInst.store,
            FReg(XReg.STACK), SImm(i),
            FReg(saves(key)))
        }
        for (Move(s, d) <- argMoves) draftMv(CM(s"[E] move arg"), d, s)
        draftCommand(cm, FInst.jal, FLabel(LAbs, fn))
        if (dest != XReg.DUMMY && dest != XReg.RETURN) {
          draftMv(NC, dest, XReg.RETURN)
        }
        for ((i, key) <- savedKeysByPosition; if saves(key) != dest) {
          draftCommand(CM(s"[E] restore local ${key.str}"), FInst.load,
            FReg(saves(key)),
            FReg(XReg.STACK), SImm(i))
        }
      case _ => !!!!(l)
    }
  }

  private[this] def emitJump(ji: JumpIndex): Unit = {
    currentFun.body.jumps(ji) match {
      case Return(cm, _, XReg.DUMMY | XReg.RETURN, _) =>
        draftRevertStack()
        draftCommand(cm, FinalInst.jr, FReg(XReg.LINK))
      case jump @ Branch(cm, _, cond, _, tru, fls) =>
        val flsLabel = blockLabel(EmitUtil.nextNonEmptyBlockIndex(currentFun.body, fls))
        cond match {
          case Branch.CondVC(op, left: XReg, right) =>
            right match {
              case V(v) =>
                draftCommand(cm,
                  FInst.negVJumpFromCmpOpVC(op), FReg(left), FReg(v.asXReg.get),
                  FLabel(LRel, flsLabel),
                )
              case C(c) =>
                if (TImm.dom contains (c.int: Int)) {
                  draftCommand(cm,
                    FInst.negCJumpFromCmpOpVC(op), FReg(left), TImm(c.int), FLabel(LRel, flsLabel),
                  )
                } else {
                  draftMvi(CM(s"[E] jump imm out of domain: $c"), XReg.LAST_TMP, c)
                  draftCommand(cm,
                    FInst.negVJumpFromCmpOpVC(op), FReg(left), FReg(XReg.LAST_TMP),
                    FLabel(LRel, flsLabel),
                  )
                }
            }
          case Branch.CondV(op, left: XReg, right: XReg) =>
            draftCommand(cm,
              FInst.negJumpFromCmpOpV(op),
              FReg(left),
              FReg(right),
              FLabel(LRel, flsLabel),
            )
          case _ => !!!!(jump)
        }
        emitBlock(tru)
        emitBlock(fls)
      case Merge(cm, _, inputs, XReg.DUMMY, output) if inputs.forall(_.xid == XReg.DUMMY) =>
        val outputLabel = blockLabel(output)
        if (inputs.forall(emittedBlocks contains _.bi)) {
          emitBlock(output)
        } else {
          draftCommand(cm, FinalInst.j, FLabel(LAbs, outputLabel))
        }
      case j => !!!!(j)
    }
  }

  private[this] def emitBlock(bi: BlockIndex) = {
    val b = currentFun.body.blocks(bi)
    draftLabel(blockLabel(bi))
    b.lines.zipWithIndex.foreach { case (line, i) =>
      val isTail =
        i == b.lines.length - 1 &&
        (currentFun.body.jumps(b.output) match {
          case _: Return => true
          case _ => false
        })
      emitLine(line, isTail)
    }
    emittedBlocks += bi
    emitJump(b.output)
  }

  private[this] def emitFunction(fun: FDef) = {
    require(fun.args == XReg.NORMAL_REGS.take(fun.args.size), fun.args)

    currentFun = fun
    emittedBlocks.clear()
    currentStack.clear()
    currentStackInv.clear()
    currentFLines.clear()
    currentStackMaxSize = MaxStackSize(0)

    draftLabel(fun.name)

    if (fun.name == ID.Special.MAIN) {
      // draftCommand(NC, FinalInst.addi, FReg(XReg.C_ONE), FReg(XReg.ZERO), SImm(1))
      draftCommand(NC, FinalInst.addi, FReg(XReg.C_MINUS_ONE), FReg(XReg.ZERO), SImm(-1))
      for ((r, w) <- XReg.toConstants -- Seq(XReg.ZERO, XReg.C_MINUS_ONE)) {
        draftMvi(NC, r, w)
      }
      // extend stackより前にスタックポインタを初期化する
      val ml2 = AnsCaml.config.memorySizeLog2
      draftCommand(CM(s"[E] bottom of stack = 2^$ml2"),
        FinalInst.orhi, FReg(XReg.STACK), FReg(XReg.ZERO), UImm(1 << ml2 - 16))
    }
    if (!fun.info.isLeaf) {
      draftCommand(CM("[E] save LR"),
        FInst.store, FReg(XReg.STACK), SImm(LinkRegOffset), FReg(XReg.LINK))
      currentFLines += { case MaxStackSize(sz) =>
        if (sz == 0) Nil
        else List(
          FinalCommand(CM("[E] extend stack"),
            FInst.addi, FReg(XReg.STACK), FReg(XReg.STACK), SImm(-sz))
        )
      }
    } else {
      Logger.log("[EM]", s"leaf: ${fun.name}")
    }

    emitBlock(fun.body.blocks.firstKey)

    fixedFLines ++= currentFLines.flatMap(_ (currentStackMaxSize))
  }

  program.functions.foreach(emitFunction)

  def writeTo(writer: PrintWriter): Unit = {
    fixedFLines.foreach(fl => writer.write(fl.toFinalString + "\n"))

    val cnt = fixedFLines.count {
      case _: FinalCommand => true
      case _: FinalLabel => false
    }
    Logger.log("EM", s"Emitted $cnt lines")
  }
}

object Emitter {

  final case class Move(src: XReg, dest: XReg)

  /** 一時レジスタを使用してレジスタの同時代入を実現 */
  def moveSimultaneously(moves: List[Move]): List[Move] = {
    val doneMoves = mutable.ListBuffer[Move]()
    // (値の保持レジスタ, 宛先レジスタ)の列
    val restMoves =
      moves.filter(m => m.src != m.dest)
        .map(m => Move(m.src.asXReg.get, m.dest.asXReg.get)).to(mutable.Set)

    while (restMoves.nonEmpty) {
      restMoves.find(m1 => restMoves.forall(m2 => m1.dest != m2.src)) match {
        case None => // restMovesの各組の宛先レジスタがほかの組の保持レジスタで埋まっているため一時レジスタに「退避」する
          assert(restMoves.forall(_.src != XReg.LAST_TMP)) // 一度に一時レジスタを使えるMoveは1つまで
          val m = restMoves.head
          doneMoves += Move(m.src, XReg.LAST_TMP)

          restMoves -= m
          restMoves += Move(XReg.LAST_TMP, m.dest)

        case Some(m) =>
          doneMoves += Move(m.src, m.dest)
          restMoves -= m
      }
    }
    doneMoves.toList
  }
}
