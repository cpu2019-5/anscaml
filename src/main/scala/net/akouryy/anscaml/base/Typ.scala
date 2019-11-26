package net.akouryy.anscaml.base

sealed trait Typ

object Typ {

  val Unit = Tuple(Nil)

  final case class Bool() extends Typ

  final case class Int() extends Typ

  final case class Float() extends Typ

  /** arguments are not curried */
  final case class Fun(args: List[Typ], ret: Typ) extends Typ

  final case class Tuple(elems: List[Typ]) extends Typ

  final case class Array(elem: Typ) extends Typ

  final case class TypVar(v: scala.Int) extends Typ


  private[this] var cnt = -1

  def generateTypVar(): TypVar = {
    cnt += 1
    TypVar(cnt)
  }
}
