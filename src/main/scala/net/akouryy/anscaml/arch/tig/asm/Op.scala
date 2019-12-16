package net.akouryy.anscaml
package arch.tig.asm

import base._

sealed trait UnOp extends Product

case object Floor extends UnOp

case object Itof extends UnOp

case object FInv extends UnOp

case object FSqrt extends UnOp

/** 即値を取れる純粋二項演算 */
sealed trait BinOpVC {
  def fn(l: Word, r: Word): Word

  val isCommutative: Boolean
}

case object Add extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word(l.int + r.int)

  override val isCommutative = true
}

case object Sha extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word(
    if (r.int >= 0) l.int << r.int
    else l.int >> -r.int
  )

  override val isCommutative = false
}

case object Band extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word(l.int & r.int)

  override val isCommutative = true
}

case object Bor extends BinOpVC {
  override def fn(l: Word, r: Word): Word = Word(l.int | r.int)

  override val isCommutative = true
}

sealed trait BinOpV extends Product {
  val toInstString: String = productPrefix.toLowerCase

  def fn(l: Word, r: Word): Word
}

case object Sub extends BinOpV {
  override def fn(l: Word, r: Word): Word = Word(l.int - r.int)
}

// TODO: remove

case object Div extends BinOpV {
  override def fn(l: Word, r: Word): Word = Word(l.int / r.int)
}

case object Fadd extends BinOpV {
  override def fn(l: Word, r: Word): Word = Word.fromFloat(l.float + r.float)
}

case object Fsub extends BinOpV {
  override def fn(l: Word, r: Word): Word = Word.fromFloat(l.float - r.float)
}

case object Fmul extends BinOpV {
  override def fn(l: Word, r: Word): Word = Word.fromFloat(l.float * r.float)
}

case object Fdiv extends BinOpV {
  override def fn(l: Word, r: Word): Word = Word.fromFloat(l.float / r.float)
}

case object FnegCond extends BinOpV {
  override def fn(l: Word, r: Word): Word = Word.fromFloat(if (r.int >= 0) l.float else -l.float)
}

case object FaddAbs extends BinOpV {
  override def fn(l: Word, r: Word): Word = Word.fromFloat((l.float + r.float).abs)
}

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
