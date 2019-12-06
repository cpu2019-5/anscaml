package net.akouryy.anscaml
package arch.tig
package emit

import java.io.PrintWriter

import asm._

import scala.collection.mutable

class Emitter {
  private[this] var writer: PrintWriter = _

  private[this] def write(ss: String*) = {
    writer.write("  " + ss.map(s => f"$s%9s").mkString(" ") + "\n")
  }

  private[this] def emitFunction(fun: FDef) = {
    require(fun.args == XReg.NORMAL_REGS.take(fun.args.size), fun.args)
  }

  def apply(writer: PrintWriter, program: Program): Unit = {
    this.writer = writer
    program.functions.foreach(emitFunction)
  }
}

object Emitter {

  final case class Move(src: XID, dest: XID)

  /** 一時レジスタを使用してレジスタの同時代入を実現 */
  def moveSimultaneously(moves: List[Move]): List[Line] = {
    val lines = mutable.ListBuffer[Line]()
    // (値の保持レジスタ, 宛先レジスタ)の列
    val restMoves =
      moves.filter(m => m.src != m.dest)
        .map(m => Move(m.src.asXReg.get, m.dest.asXReg.get)).to(mutable.Set)

    while (restMoves.nonEmpty) {
      restMoves.find(m1 => restMoves.forall(m2 => m1.dest != m2.src)) match {
        case None => // restMovesの各組の宛先レジスタがほかの組の保持レジスタで埋まっているため一時レジスタに「退避」する
          assert(restMoves.forall(_.src != XReg.REG_LAST_TMP)) // 一度に一時レジスタを使えるMoveは1つまで
          val m = restMoves.head
          lines += Line(XReg.REG_LAST_TMP, Mv(m.src))

          restMoves -= m
          restMoves += Move(XReg.REG_LAST_TMP, m.dest)

        case Some(m) =>
          lines += Line(m.dest, Mv(m.src))
          restMoves -= m
      }
    }
    lines.toList
  }
}
