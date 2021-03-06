package net.akouryy.anscaml
package arch.tig.asm

import base._

sealed trait VC {
  final def asV: Option[XID] = this match {
    case V(v) => Some(v)
    case _: C => None
  }

  final def asVXVar: Option[XVar] = this.asV.flatMap(_.asXVar)

  final def fold[T](ifV: XID => T, ifC: Word => T): T = this match {
    case V(v) => ifV(v)
    case C(c) => ifC(c)
  }

  final def mapV(ifV: XID => XID): VC = fold(v => V(ifV(v)), C)
}

object VC {

  object Immutable {
    def unapply(vc: VC): Option[VC] = vc match {
      case V(XID.Immutable(_)) | C(_) => Some(vc)
      case _ => None
    }
  }

}


final case class V(v: XID) extends VC

final case class C(c: Word) extends VC

object C extends (Word => C) {
  def int(i: Int) = C(Word(i))

  def float(f: Float) = C(Word.fromFloat(f))
}
