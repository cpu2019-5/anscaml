package net.akouryy.anscaml
package arch.tig.asm

sealed trait VC

final case class V(v: AID) extends VC

final case class C(c: Int) extends VC
