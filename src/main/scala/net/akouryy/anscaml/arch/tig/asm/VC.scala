package net.akouryy.anscaml
package arch.tig.asm

import base._

sealed trait VC {
  def vOpt: Option[AID] = this match {
    case V(v) => Some(v)
    case _: C => None
  }

  def vAVarOpt: Option[AVar] = this.vOpt.flatMap(_.aVarOpt)

  def fold[T](ifV: AID => T, ifC: Word => T): T = this match {
    case V(v) => ifV(v)
    case C(c) => ifC(c)
  }
}

final case class V(v: AID) extends VC

final case class C(c: Word) extends VC
