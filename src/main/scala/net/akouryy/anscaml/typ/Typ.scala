package net.akouryy.anscaml
package typ

import base._

sealed trait Typ {
  def |(typ: Typ): Typ = (this, typ) match {
    case (Typ.TBool(l1), Typ.TBool(l2)) => Typ.TBool((l1 | l2) (1))
    case (Typ.TInt(l1), Typ.TInt(l2)) => Typ.TInt((l1 | l2) (2))
    case (Typ.TFloat(l1), Typ.TFloat(l2)) => Typ.TFloat((l1 | l2) (1))
    case (Typ.TFun(args1, ret1), Typ.TFun(args2, ret2)) =>
      Typ.TFun(args1.zipMap(args2)(_ | _), ret1 | ret2)
    case (Typ.TTuple(elems1), Typ.TTuple(elems2)) => Typ.TTuple(elems1.zipMap(elems2)(_ | _))
    case (Typ.TArray(elem1), Typ.TArray(elem2)) => Typ.TArray(elem1 | elem2)
    case (_, _) => println(this, typ); ???
  }

  def recursively(convert: Typ => Typ): Typ = this match {
    case Typ.TBool(_) | Typ.TInt(_) | Typ.TFloat(_) | Typ.TypVar(_) => this
    case Typ.TFun(args, ret) => Typ.TFun(args.map(convert), convert(ret))
    case Typ.TTuple(elems) => Typ.TTuple(elems.map(convert))
    case Typ.TArray(elem) => Typ.TArray(convert(elem))
  }

  def freeVariables: Set[Typ.TypVar] = this match {
    case Typ.TBool(_) | Typ.TInt(_) | Typ.TFloat(_) => Set()
    case Typ.TFun(args, ret) => args.map(_.freeVariables).foldLeft(ret.freeVariables)(_ | _)
    case Typ.TTuple(elems) => elems.map(_.freeVariables).foldLeft(Set[Typ.TypVar]())(_ | _)
    case Typ.TArray(elem) => elem.freeVariables
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
  final case class TFun(args: List[Typ], ret: Typ) extends Typ

  final case class TTuple(elems: List[Typ]) extends Typ

  final case class TArray(elem: Typ) extends Typ

  final case class TypVar(num: Int) extends Typ

  val TUnit = TTuple(Nil)
  val BoolAll = TBool(Lit.All())
  val IntAll = TInt(Lit.All())
  val FloatAll = TFloat(Lit.All())

  def BoolList(list: Boolean*) = Typ.TBool(Lit.List[Primitives.PBool](Set(list: _*)))

  def IntList(list: Int*) = Typ.TInt(Lit.List[Primitives.PInt](Set(list: _*)))

  def FloatList(list: Float*) = Typ.TFloat(Lit.List[Primitives.PFloat](Set(list: _*)))

  private[this] var cnt = -1

  def generateTypVar(): TypVar = {
    cnt += 1
    TypVar(cnt)
  }
}
