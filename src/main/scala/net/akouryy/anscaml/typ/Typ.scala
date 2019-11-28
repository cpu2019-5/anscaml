package net.akouryy.anscaml
package typ

import base._

sealed trait Typ {
  def recursively(convert: Typ => Typ): Typ = this match {
    case Typ.TBool(_) | Typ.TInt(_) | Typ.TFloat(_) | Typ.TypVar(_) => this
    case Typ.Fun(args, ret) => Typ.Fun(args.map(convert), convert(ret))
    case Typ.Tuple(elems) => Typ.Tuple(elems.map(convert))
    case Typ.Array(elem) => Typ.Array(convert(elem))
  }

  def freeVariables: Set[Typ.TypVar] = this match {
    case Typ.TBool(_) | Typ.TInt(_) | Typ.TFloat(_) => Set()
    case Typ.Fun(args, ret) => args.map(_.freeVariables).foldLeft(ret.freeVariables)(_ | _)
    case Typ.Tuple(elems) => elems.map(_.freeVariables).foldLeft(Set[Typ.TypVar]())(_ | _)
    case Typ.Array(elem) => elem.freeVariables
    case typ: Typ.TypVar => Set(typ)
  }

  def withNewVars(): Typ = this match {
    case Typ.TBool(_) => Typ.TBool(Lit.Var.generate())
    case Typ.TInt(_) => Typ.TInt(Lit.Var.generate())
    case Typ.TFloat(_) => Typ.TFloat(Lit.Var.generate())
    case Typ.TypVar(_) => Typ.generateTypVar()
    case _ => recursively(_.withNewVars())
  }
}

object Typ {

  sealed trait Prim[T <: Primitives.IFB] extends Typ {
    val lit: Lit[T]
  }

  object Prim {
    def apply(lit: Lit[Primitives.PBool]) = TBool(lit)

    def apply(lit: Lit[Primitives.PInt]) = TInt(lit)

    def apply(lit: Lit[Primitives.PFloat]) = TFloat(lit)
  }

  final case class TBool(lit: Lit[Primitives.PBool]) extends Prim[Primitives.PBool]

  final case class TInt(lit: Lit[Primitives.PInt]) extends Prim[Primitives.PInt]

  final case class TFloat(lit: Lit[Primitives.PFloat]) extends Prim[Primitives.PFloat]

  /** arguments are not curried */
  final case class Fun(args: List[Typ], ret: Typ) extends Typ

  final case class Tuple(elems: List[Typ]) extends Typ

  final case class Array(elem: Typ) extends Typ

  final case class TypVar(num: scala.Int) extends Typ

  val TUnit = Tuple(Nil)
  val BoolAll = TBool(Lit.All())
  val IntAll = TInt(Lit.All())
  val FloatAll = TFloat(Lit.All())

  private[this] var cnt = -1

  def generateTypVar(): TypVar = {
    cnt += 1
    TypVar(cnt)
  }
}
