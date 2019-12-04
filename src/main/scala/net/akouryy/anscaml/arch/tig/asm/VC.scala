package net.akouryy.anscaml
package arch.tig.asm

sealed trait VC {
  def vOpt: Option[AID] = this match {
    case V(v) => Some(v)
    case _: C => None
  }
}

final case class V(v: AID) extends VC

final case class C(c: Int) extends VC
