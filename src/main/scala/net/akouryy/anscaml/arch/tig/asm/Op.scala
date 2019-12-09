package net.akouryy.anscaml
package arch.tig.asm

import base._

sealed trait UnOp extends Product

case object Floor extends UnOp

case object Itof extends UnOp

/** 即値を取れる純粋二項演算 */
sealed trait BinOpVC {
  def fn(l: Word, r: Word): Word
}

case object Add extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word.fromInt(l.int + r.int)
}

case object Sha extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word.fromInt(
    if (r.int >= 0) l.int << r.int
    else l.int >> -r.int
  )
}

case object Band extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word.fromInt(l.int & r.int)
}

case object Bor extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word.fromInt(l.int | r.int)
}

sealed trait BinOpV extends Product {
  val toInstString: String = productPrefix.toLowerCase
}

case object Sub extends BinOpV

// TODO: remove

case object Div extends BinOpV

case object Fadd extends BinOpV

case object Fsub extends BinOpV

case object Fmul extends BinOpV

case object Fdiv extends BinOpV

case object FnegCond extends BinOpV

sealed trait CmpOp {
  def fn(l: Word, r: Word): Boolean
}

sealed trait CmpOpVC extends CmpOp

case object Eq extends CmpOpVC {
  override def fn(l: Word, r: Word): Boolean = l.int == r.int
}

case object Le extends CmpOpVC {
  override def fn(l: Word, r: Word): Boolean = l.int <= r.int
}

sealed trait CmpOpV extends CmpOp

case object FLe extends CmpOpV {
  override def fn(l: Word, r: Word): Boolean = l.float <= r.float
}

object CmpOpVC {
  def fromSyntax(op: syntax.CmpOp): Either[CmpOpV, CmpOpVC] = op match {
    case syntax.CmpOp.Eq | syntax.CmpOp.Feq => Right(Eq)
    case syntax.CmpOp.Le => Right(Le)
    case syntax.CmpOp.Fle => Left(FLe)
  }
}
