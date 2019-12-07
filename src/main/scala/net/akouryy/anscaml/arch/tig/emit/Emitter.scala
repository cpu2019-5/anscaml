package net.akouryy.anscaml
package arch.tig
package emit

import java.io.PrintWriter

import asm._
import base._

import scala.collection.mutable

class Emitter {

  import Emitter._

  private[this] var writer: PrintWriter = _

  private[this] var currentFun: FDef = _
  private[this] var emittedBlocks = mutable.Set[BlockIndex]()
  private[this] var currentStack = mutable.ArrayBuffer[ID]()
  private[this] var currentStackInv = mutable.Map[ID, Int]()

  private[this] def write(ss: Any*) = {
    writer.write("  " + ss.map(s => f"$s%9s").mkString(" ") + "\n")
  }

  private[this] def writeLabel(label: String) = writer.write(label + ":" + "\n")

  private[this] def blockLabel(prefix: String, bi: BlockIndex) =
    s"$prefix${currentFun.name.name}.${bi.indexString}"

  private[this] def emitMv(dest: XReg, src: XReg) = {
    write("band", dest, XReg.C_MINUS_ONE, src)
  }

  private[this] def emitLine(l: Line): Unit = {
    val dest = l.dest.asXReg.get
    l.inst match {
      case Mv(id: XReg) => emitMv(dest, id)
      case Mvi(value) =>
        val lowerMask = (1 << 16) - 1
        val higher = value.int >> 16
        val lower = value.int & lowerMask
        val higherLHS =
          if (lower != 0) {
            write("bandi", dest, XReg.C_MINUS_ONE, lower)
            dest
          } else {
            XReg.ZERO
          }
        if (higher != 0 || lower == 0) { // lower==0のときも入れないとdestへの代入が一切行われないままになる
          write("orhi", dest, higherLHS, higher)
        }
      case NewArray(V(len: XReg), elem: XReg) =>
        // do-whileループでtmpが0以上である間拡張を繰り返す
        // len=0のとき1回ループしてしまうが、未定義領域に1個書き込むだけなので許容
        val bodyLabel =
          ID.generate(ID(s"${currentFun.name.name}.${ID.Special.EMIT_ARRAY_BODY}")).str
        emitMv(XReg.LAST_TMP, len)
        writeLabel(bodyLabel)
        write("store", XReg.HEAP, 0, elem)
        write("addi", XReg.HEAP, XReg.HEAP, 1)
        write("addi", XReg.LAST_TMP, XReg.LAST_TMP, -1)
        write("jgt", XReg.LAST_TMP, XReg.ZERO, s"rel:$bodyLabel")
        write("sub", dest, XReg.HEAP, len)
      case NewArray(C(len), elem: XReg) =>
        for (i <- 0 until len.int) {
          write("store", XReg.HEAP, i, elem)
        }
        emitMv(dest, XReg.HEAP)
        write("addi", XReg.HEAP, XReg.HEAP, len)
      case Store(addr: XReg, index, value: XReg) if dest == XReg.DUMMY =>
        write("store", addr, index.c.int, value)
      case Load(addr: XReg, V(index: XReg)) => write("loadreg", dest, addr, index)
      case Load(addr: XReg, C(index)) => write("load", dest, addr, index.int)
      case UnOpTree(op, value: XReg) => write(op.toInstString, dest, value)
      case BinOpVCTree(op, left: XReg, right @ (V(_: XReg) | _: C)) =>
        write(
          right.fold(_ => op.toInstString, _ => op.toImmInstString),
          dest,
          left,
          right.fold(identity, _.int),
        )
      case BinOpVTree(op, left: XReg, right: XReg) => write(op.toInstString, dest, left, right)
      case Nop if l.dest == XReg.DUMMY => // nop
      case Read => write("read", dest)
      case Write(value: XReg) if l.dest == XReg.DUMMY => write("write", value)
      case CallDir(fn, args, Some(saves)) =>
        // TODO: tail call
        val savedKeyPositions = saves.keys.flatMap { k =>
          currentStackInv.get(k).map(k -> _)
        }.to(mutable.Map)
        val savedKeysByPosition = savedKeyPositions.map(_.swap)

        for {
          (key, reg) <- saves
          if !savedKeyPositions.contains(key)
        } {
          val i = (0 until saves.size).find(i => !savedKeysByPosition.contains(i)).get
          savedKeyPositions(key) = i
          savedKeysByPosition(i) = key
          if (i < currentStack.size) {
            currentStack(i) = key
          } else {
            assert(i == currentStack.size)
            currentStack += key
          }
          currentStackInv(key) = i
        }
        println(fn)
        println(saves)
        println(currentStack)

        val otherStackSpaceSize = 1 // リンクレジスタ退避用
        val saveSize = saves.size max savedKeyPositions.values.maxOption.foldF(_ + 1, 0)
        val argMoves = moveSimultaneously(
          args.zipWithIndex.map {
            case (a, i) => Move(a.asXReg.get, XReg.NORMAL_REGS(i))
          }
        )

        write("store", XReg.STACK, -1, XReg.LINK)
        for ((i, key) <- savedKeysByPosition) {
          write("store", XReg.STACK, -(otherStackSpaceSize + i + 1), saves(key))
        }
        write("addi", XReg.STACK, XReg.STACK, -(otherStackSpaceSize + saveSize))
        for (Move(s, d) <- argMoves) emitMv(d, s)
        write("jal", fn.name)
        write("addi", XReg.STACK, XReg.STACK, otherStackSpaceSize + saveSize)
        for ((i, key) <- savedKeysByPosition) {
          write("load", saves(key), XReg.STACK, -(otherStackSpaceSize + i + 1))
        }
        write("load", XReg.LINK, XReg.STACK, -1)
        if (dest != XReg.DUMMY && dest != XReg.RETURN) {
          emitMv(dest, XReg.RETURN)
        }
      case _ => ????(l)
    }
  }

  private[this] def emitJump(ji: JumpIndex): Unit = {
    val retReg = XReg.NORMAL_REGS(0)

    currentFun.body.jumps(ji) match {
      case Return(_, XReg.DUMMY | `retReg`, _) =>
        write("jr", XReg.LINK)
      case Condition(_, op, left: XReg, right @ (V(_: XReg) | _: C), _, tru, fls) =>
        write(
          right.fold(_ => op.toNegJumpString, _ => op.toNegImmJumpString),
          left,
          right.fold(identity, _.int),
          blockLabel("rel:", fls)
        )
        emitBlock(tru)
        writeLabel(blockLabel("", fls))
        emitBlock(fls)
      case Merge(_, inputs, XReg.DUMMY, output) if inputs.forall(_._1 == XReg.DUMMY) =>
        if (inputs.forall(emittedBlocks contains _._2)) {
          writeLabel(blockLabel("", output))
          emitBlock(output)
        } else {
          write("j", blockLabel("", output))
        }
      case j => require(requirement = false, j)
    }
  }

  private[this] def emitBlock(bi: BlockIndex) = {
    val b = currentFun.body.blocks(bi)
    writeLabel(blockLabel("", bi))
    b.lines.foreach(emitLine)
    emittedBlocks += bi
    emitJump(b.output)
  }

  private[this] def emitFunction(fun: FDef) = {
    require(fun.args == XReg.NORMAL_REGS.take(fun.args.size), fun.args)

    writeLabel(fun.name.name)

    if (fun.name.name == ID.Special.MAIN) {
      write("addi", XReg.C_ONE, XReg.ZERO, 1)
      write("addi", XReg.C_MINUS_ONE, XReg.ZERO, -1)
    }

    currentFun = fun
    emittedBlocks.clear()
    currentStack.clear()
    currentStackInv.clear()

    emitBlock(fun.body.blocks.firstKey)
  }

  def apply(writer: PrintWriter, program: Program): Unit = {
    this.writer = writer
    program.functions.foreach(emitFunction)
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
