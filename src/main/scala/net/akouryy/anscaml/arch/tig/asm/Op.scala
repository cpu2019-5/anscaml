package net.akouryy.anscaml
package arch.tig.asm

import base._

sealed trait UnOp

case object Floor extends UnOp

case object Itof extends UnOp

/** 即値を取れる純粋二項演算 */
sealed trait BinOpVC {
  def fn(l: Word, r: Word): Word
}

case object Add extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word.fromInt(l.int + r.int)
}

case object Sub extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word.fromInt(l.int - r.int)
}

case object Sha extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word.fromInt(
    if (r.int >= 0) l.int << r.int
    else l.int >> -r.int
  )
}

case object Land extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word.fromInt(l.int & r.int)
}

case object Lor extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word.fromInt(l.int | r.int)
}

sealed trait BinOpV

case object Fadd extends BinOpV

case object Fsub extends BinOpV

case object Fmul extends BinOpV

case object Fdiv extends BinOpV

case object FnegCond extends BinOpV

sealed trait CmpOp {
  def fn(l: Word, r: Word): Boolean
}

case object Eq extends CmpOp {
  override def fn(l: Word, r: Word): Boolean = l.int == r.int
}

case object Le extends CmpOp {
  override def fn(l: Word, r: Word): Boolean = l.int <= r.int
}

case object FLe extends CmpOp {
  override def fn(l: Word, r: Word): Boolean = l.float <= r.float
}

object CmpOp {
  def fromSyntax(op: syntax.CmpOp): CmpOp = op match {
    case syntax.CmpOp.Eq | syntax.CmpOp.Feq => Eq
    case syntax.CmpOp.Le => Le
    case syntax.CmpOp.Fle => FLe
  }
}
