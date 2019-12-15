package net.akouryy.anscaml
package arch.tig.asm

import net.akouryy.anscaml.typ.Typ

/** 関数を含まないアセンブリの型 */
sealed trait Ty

case object TyUnit extends Ty

case object TyWord extends Ty

final case class TyPointer(to: Ty) extends Ty

final case class TyTuple(elems: List[Ty]) extends Ty

final case class TyArray(elem: Ty) extends Ty

object Ty {
  def apply(typ: Typ): Ty = typ match {
    case Typ.TUnit => TyUnit
    case Typ.TInt(_) | Typ.TFloat(_) => TyWord
    case Typ.TTuple(elems) => TyPointer(TyTuple(elems map apply))
    case Typ.TArray(elem) => TyPointer(TyArray(Ty(elem)))
    case Typ.TBool(_) | Typ.TFun(_, _) | Typ.TypVar(_) =>
      throw new RuntimeException(s"[Tig Ty] cannot convert $typ")
  }
}

final case class Fn(args: List[Ty], ret: Ty, safeRegs: Set[XReg])

object Fn {
  def fromTyp(typ: Typ): Fn = typ match {
    case Typ.TFun(args, ret) => Fn(args map Ty.apply, Ty(ret), Set())
    case _ => throw new RuntimeException(s"[Tig Ty] cannot convert $typ")
  }
}
