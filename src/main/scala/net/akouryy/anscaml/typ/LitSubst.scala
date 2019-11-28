package net.akouryy.anscaml
package typ

import base._

import scala.collection.mutable
import scala.reflect.ClassTag

final class LitSubst[T <: Primitives.IFB : ClassTag](val mapping: mutable.Map[Lit.Var[T], Lit[T]]) {
  def *(lit: Lit[T]): Lit[T] = lit match {
    case lit: Lit.Var[T] => mapping.getOrElse(lit, lit)
    case _ => lit
  }

  def *(typ: Typ): Typ = typ match {
    case Typ.TBool(lit) =>
      if (implicitly[ClassTag[T]].runtimeClass.isInstance(Primitives.PBool))
        Typ.TBool(*(lit.asInstanceOf[Lit[T]]).asInstanceOf[Lit[Primitives.PBool]])
      else
        typ
    case Typ.TInt(lit) =>
      if (implicitly[ClassTag[T]].runtimeClass.isInstance(Primitives.PInt))
        Typ.TInt(*(lit.asInstanceOf[Lit[T]]).asInstanceOf[Lit[Primitives.PInt]])
      else
        typ
    case Typ.TFloat(lit) =>
      if (implicitly[ClassTag[T]].runtimeClass.isInstance(Primitives.PFloat))
        Typ.TFloat(*(lit.asInstanceOf[Lit[T]]).asInstanceOf[Lit[Primitives.PFloat]])
      else
        typ
    case _ => typ.recursively(*)
  }

  def *(tsb: TypSubst): TypSubst =
    new TypSubst(mutable.Map(tsb._1.view.mapValues(*).toSeq: _*))

  import Constrainer._

  def *(tcr: Constraint): Constraint = tcr match {
    case t1 >:> t2 => *(t1) >:> *(t2)
    case t1 =:= t2 => *(t1) =:= *(t2)
  }
}
