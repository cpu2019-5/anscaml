package net.akouryy.anscaml
package typ

import base._

sealed trait Lit[T <: Primitives.IFB] {
  def |(lit: => Lit[T])(limit: Int): Lit[T] = {
    this match {
      case Lit.Var(_) => ???
      case Lit.List(ls1) => lit match {
        case Lit.Var(_) => ???
        case Lit.All() => Lit.All()
        case Lit.List(ls2) =>
          val ls = ls1 | ls2
          if (ls.sizeIs <= limit) {
            Lit.List(ls)
          } else {
            Lit.All()
          }
      }
      case Lit.All() => Lit.All()
    }
  }
}

object Lit {

  final case class Var[T <: Primitives.IFB](i: scala.Int) extends Lit[T]

  final case class List[T <: Primitives.IFB](literals: Set[T#T]) extends Lit[T]

  final case class All[T <: Primitives.IFB]() extends Lit[T]

  def Empty[T <: Primitives.IFB]: List[T] = List(Set())

  object Var {
    private[this] var cnt = -1

    def generate[T <: Primitives.IFB](): Var[T] = {
      cnt += 1
      Var(cnt)
    }

    def count: Int = cnt + 1
  }

}
