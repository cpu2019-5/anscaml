package net.akouryy.anscaml
package arch.tig
package emit

import java.io.PrintWriter

import asm._
import base._
import emit.{FinalInst => FInst}
import FinalArg.{Imm => FImm, Label => FLabel, LAbs, LRel}

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
  private[this] val currentFLines =
    mutable.ListBuffer[MaxStackSize => FinalLine]()

  private[this] def draft(fLine: FinalLine): Unit = currentFLines += (_ => fLine)

  private[this] def draftCommand(comment: String, inst: FInst, args: FinalArg*): Unit =
    draft(FinalCommand(comment, inst, args: _*))

  private[this] def draftLabel(label: String): Unit = currentFLines += (_ => FinalLabel(label, ""))

  private[this] def FReg(xReg: XReg) = FinalArg.Reg(xReg.toString)

  private[this] def blockLabel(bi: BlockIndex) =
    s"${currentFun.name}.${bi.indexString}"

  private[this] def draftMv(dest: XReg, src: XReg) =
    draftCommand("", FInst.band, FReg(dest), FReg(XReg.C_MINUS_ONE), FReg(src))

  private[this] def draftRevertStack(): Unit = {
    currentFLines += { case MaxStackSize(sz) =>
      FinalCommand("", FInst.addi, FReg(XReg.STACK), FReg(XReg.STACK), FImm(+sz))
    }
    draftCommand("", FInst.load, FReg(XReg.LINK), FReg(XReg.STACK), FImm(LinkRegOffset))
  }

  private[this] def emitLine(l: Line): Unit = {
    val dest = l.dest.asXReg.get
    val toDummy = l.dest == XReg.DUMMY
    l.inst match {
      case Mv(id: XReg) if !toDummy => draftMv(dest, id)
      case Mvi(value) if !toDummy =>
        val lowerMask = (1 << 16) - 1
        val higher = value.int >> 16
        val lower = value.int & lowerMask
        val higherLHS =
          if (lower != 0) {
            draftCommand("", FInst.bandi, FReg(dest), FReg(XReg.C_MINUS_ONE), FImm(lower))
            dest
          } else {
            XReg.ZERO
          }
        if (higher != 0 || lower == 0) { // lower==0のときも入れないとdestへの代入が一切行われないままになる
          draftCommand("", FInst.orhi, FReg(dest), FReg(higherLHS), FImm(higher))
        }
      case NewArray(V(len: XReg), elem: XReg) if !toDummy =>
        // do-whileループでtmpが0以上である間拡張を繰り返す
        // len=0のとき1回ループしてしまうが、未定義領域に1個書き込むだけなので許容
        val bodyLabel =
          ID.generate(ID(s"${currentFun.name}.${ID.Special.EMIT_ARRAY_BODY}")).str
        draftMv(XReg.LAST_TMP, len)
        draftLabel(bodyLabel)
        draftCommand("", FInst.store, FReg(XReg.HEAP), FImm(0), FReg(elem))
        draftCommand("", FInst.addi, FReg(XReg.HEAP), FReg(XReg.HEAP), FImm(1))
        draftCommand("", FInst.addi, FReg(XReg.LAST_TMP), FReg(XReg.LAST_TMP), FImm(-1))
        draftCommand("", FInst.jgt,
          FReg(XReg.LAST_TMP), FReg(XReg.ZERO), FLabel(LRel, bodyLabel))
        draftCommand("", FInst.sub, FReg(dest), FReg(XReg.HEAP), FReg(len))
      case NewArray(C(len), elem: XReg) if !toDummy =>
        for (i <- 0 until len.int) {
          draftCommand("", FInst.store, FReg(XReg.HEAP), FImm(i), FReg(elem))
        }
        draftMv(dest, XReg.HEAP)
        draftCommand("", FInst.addi, FReg(XReg.HEAP), FReg(XReg.HEAP), FImm(len.int))
      case Store(addr: XReg, index, value: XReg) if toDummy =>
        draftCommand("", FInst.store, FReg(addr), FImm(index.c.int), FReg(value))
      case Load(addr: XReg, V(index: XReg)) if !toDummy =>
        draftCommand("", FInst.loadreg, FReg(dest), FReg(addr), FReg(index))
      case Load(addr: XReg, C(index)) if !toDummy =>
        draftCommand("", FInst.load, FReg(dest), FReg(addr), FImm(index.int))
      case UnOpTree(op, value: XReg) if !toDummy =>
        draftCommand("", FInst.fromUnOp(op), FReg(dest), FReg(value))
      case BinOpVCTree(op, left: XReg, right @ (V(_: XReg) | _: C)) if !toDummy =>
        draftCommand("",
          right.fold(_ => FInst.vFromBinOpVC(op), _ => FInst.cFromBinOpVC(op)),
          FReg(dest),
          FReg(left),
          right.fold(v => FReg(v.asXReg.get), c => FImm(c.int)),
        )
      case BinOpVTree(op, left: XReg, right: XReg) if !toDummy =>
        draftCommand("", FInst.fromBinOpV(op), FReg(dest), FReg(left), FReg(right))
      case Nop if toDummy => // nop
      case Read =>
        draftCommand("", FInst.read, FReg(if (toDummy) XReg.ZERO else dest))
      case Write(value: XReg) if toDummy =>
        draftCommand("", FInst.write, FReg(value))
      case CallDir(ID.Special.ASM_EXIT_FUN, Nil, Some(_)) if toDummy =>
        draftCommand("", FInst.exit)
      case CallDir(fn, args, Some(saves)) /* destはdummyでもそうでなくてもよい */ =>
        // TODO: tail call
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

        val argMoves = moveSimultaneously(
          args.zipWithIndex.map {
            case (a, i) => Move(a.asXReg.get, XReg.NORMAL_REGS(i))
          }
        )

        for ((i, key) <- newSaves) {
          draftCommand("", FInst.store,
            FReg(XReg.STACK), FImm(i),
            FReg(saves(key)))
        }
        for (Move(s, d) <- argMoves) draftMv(d, s)
        draftCommand("", FInst.jal, FLabel(LAbs, fn))
        if (dest != XReg.DUMMY && dest != XReg.RETURN) {
          draftMv(dest, XReg.RETURN)
        }
        for ((i, key) <- savedKeysByPosition; if saves(key) != dest) {
          draftCommand("", FInst.load,
            FReg(saves(key)),
            FReg(XReg.STACK), FImm(i))
        }
      case _ => ????(l)
    }
  }

  private[this] def emitJump(ji: JumpIndex): Unit = {
    val retReg = XReg.NORMAL_REGS(0)

    currentFun.body.jumps(ji) match {
      case Return(_, XReg.DUMMY | `retReg`, _) =>
        draftRevertStack()
        draftCommand("", FinalInst.jr, FReg(XReg.LINK))
      case jump @ Branch(_, cond, _, tru, fls) =>
        val flsLabel = blockLabel(fls)
        cond match {
          case Branch.CondVC(op, left: XReg, right) =>
            draftCommand("",
              right.fold(_ => FInst.negVJumpFromCmpOpVC(op), _ => FInst.negCJumpFromCmpOpVC(op)),
              FReg(left),
              right.fold(v => FReg(v.asXReg.get), c => FImm(c.int)),
              FLabel(LRel, flsLabel),
            )
          case Branch.CondV(op, left: XReg, right: XReg) =>
            draftCommand("",
              FInst.negJumpFromCmpOpV(op),
              FReg(left),
              FReg(right),
              FLabel(LRel, flsLabel),
            )
          case _ => ????(jump)
        }
        emitBlock(tru)
        emitBlock(fls)
      case Merge(_, inputs, XReg.DUMMY, output) if inputs.forall(_._1 == XReg.DUMMY) =>
        val outputLabel = blockLabel(output)
        if (inputs.forall(emittedBlocks contains _._2)) {
          emitBlock(output)
        } else {
          draftCommand("", FinalInst.j, FLabel(LAbs, outputLabel))
        }
      case j => ????(j)
    }
  }

  private[this] def emitBlock(bi: BlockIndex) = {
    val b = currentFun.body.blocks(bi)
    draftLabel(blockLabel(bi))
    b.lines.foreach(emitLine)
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

    draftLabel(fun.name)

    if (fun.name == ID.Special.MAIN) {
      draftCommand("", FinalInst.addi, FReg(XReg.C_ONE), FReg(XReg.ZERO), FImm(1))
      draftCommand("", FinalInst.addi, FReg(XReg.C_MINUS_ONE), FReg(XReg.ZERO), FImm(-1))
      draftMv(XReg.STACK, XReg.ZERO)
      draftCommand("", FinalInst.orhi, FReg(XReg.HEAP), FReg(XReg.ZERO),
        FImm(1 << AnsCaml.config.memorySizeLog2 - 16))
    }
    draftCommand("", FInst.store, FReg(XReg.STACK), FImm(LinkRegOffset), FReg(XReg.LINK))
    currentFLines += { case MaxStackSize(sz) =>
      FinalCommand("", FInst.addi, FReg(XReg.STACK), FReg(XReg.STACK), FImm(-sz))
    }

    emitBlock(fun.body.blocks.firstKey)

    fixedFLines ++= currentFLines.map(_ (MaxStackSize(StackMetaDataSize + currentStack.size)))
  }

  program.functions.foreach(emitFunction)

  def writeTo(writer: PrintWriter): Unit = {
    fixedFLines.foreach(fl => writer.write(fl.toFinalString + "\n"))
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
