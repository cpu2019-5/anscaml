package net.akouryy.anscaml.arch.tig.asm

sealed trait UnOp

case object Floor extends UnOp

case object Itof extends UnOp

/** 即値を取れる純粋二項演算 */
sealed trait BinOpVC

case object Add extends BinOpVC

case object Sub extends BinOpVC

case object Shla extends BinOpVC

case object Shra extends BinOpVC

case object Land extends BinOpVC

sealed trait BinOpV

case object Fadd extends BinOpV

case object Fsub extends BinOpV

case object Fmul extends BinOpV

case object Fdiv extends BinOpV

case object FnegCond extends BinOpV
