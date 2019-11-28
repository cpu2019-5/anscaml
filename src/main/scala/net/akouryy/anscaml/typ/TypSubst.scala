package net.akouryy.anscaml
package typ

import base._
import syntax.Syntax

import scala.collection.mutable

final class TypSubst(val _1: mutable.Map[Typ.TypVar, Typ])
  extends Product1[mutable.Map[Typ.TypVar, Typ]] {

  def *(typ: Typ): Typ = typ match {
    case typ: Typ.TypVar => _1.getOrElse(typ, typ)
    case _ => typ.recursively(*)
  }

  def <<=(s: TypSubst): this.type = {
    assert(s._1.sizeIs == 1)
    _1.mapValuesInPlace((_, t) => s * t) ++= s._1
    this
  }

  override def productPrefix: String = "TypSubst"

  override def canEqual(that: Any): Boolean = that.isInstanceOf[TypSubst]

  def *(entry: Entry): Entry = Entry(entry.name, *(entry.typ))

  def *(ast: Syntax): Syntax = {
    import Syntax._

    ast match {
      case Let(Entry(name, typ), bound, kont) =>
        Let(Entry(name, *(typ)), *(bound), *(kont))
      case LetRec(FDef(Entry(fn, ft), args, body, noInline), kont) =>
        LetRec(FDef(Entry(fn, *(ft)), args.map(*), *(body), noInline), *(kont))
      case LetTuple(elems, bound, kont) =>
        LetTuple(elems.map(*), *(bound), *(kont))
      case _ => ast.recursively(*)
    }
  }

  import Constrainer._

  def *(tcr: Constraint): Constraint = tcr match {
    case t1 >:> t2 => *(t1) >:> *(t2)
    case t1 =:= t2 => *(t1) =:= *(t2)
  }
}
