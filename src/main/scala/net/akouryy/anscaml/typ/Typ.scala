package net.akouryy.anscaml
package typ

import base._

sealed trait Typ

object Typ {

  sealed trait Lit[T <: Primitives.IntOrFloatOrBool] {
    def |(lit: Lit[T]): Lit[T] = {
      this match {
        case Lit.Var(_) => ???
        case Lit.List(ls1) => lit match {
          case Lit.Var(_) => ???
          case Lit.All() => Lit.All()
          case Lit.List(ls2) => Lit.List(ls1 | ls2)
        }
        case Lit.All() => Lit.All()
      }
    }
  }

  object Lit {

    final case class Var[T <: Primitives.IntOrFloatOrBool](i: scala.Int) extends Lit[T]

    final case class List[T <: Primitives.IntOrFloatOrBool](literals: Set[T#T]) extends Lit[T]

    final case class All[T <: Primitives.IntOrFloatOrBool]() extends Lit[T]

  }

  final case class Bool(lit: Lit[Primitives.Bool]) extends Typ

  final case class Int(lit: Lit[Primitives.Int]) extends Typ

  final case class Float(lit: Lit[Primitives.Float]) extends Typ

  /** arguments are not curried */
  final case class Fun(args: List[Typ], ret: Typ) extends Typ

  final case class Tuple(elems: List[Typ]) extends Typ

  final case class Array(elem: Typ) extends Typ

  final case class TypVar(v: scala.Int) extends Typ

  val Unit = Tuple(Nil)
  val BoolAll = Bool(Lit.All())
  val IntAll = Int(Lit.All())
  val FloatAll = Float(Lit.All())

  private[this] var cnt = -1

  def generateTypVar(): TypVar = {
    cnt += 1
    TypVar(cnt)
  }
}
