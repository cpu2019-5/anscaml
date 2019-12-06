package net.akouryy.anscaml
package arch.tig.asm

import base._
import shapeless._

sealed trait Instruction {
  def mapAID(fn: AID => AID): Instruction = {
    object inc extends Poly1 {
      implicit def caseAID = at[AID](fn)

      implicit def caseAReg = at[AReg](fn)

      implicit def caseAVar = at[AVar](fn)
    }
    val res = everywhere(inc)(this) // shapelessの?バグのため一時変数に代入
    res
  }
}

final case class Mv(id: AID) extends Instruction

final case class Mvi(value: Int) extends Instruction

final case class Fmvi(value: Float) extends Instruction

final case class NewArray(len: VC, elem: AID) extends Instruction

final case class Store(addr: AID, index: VC, value: AID) extends Instruction

final case class Load(addr: AID, index: VC) extends Instruction

final case class UnOpTree(op: UnOp, value: AID) extends Instruction

final case class BinOpVCTree(op: BinOpVC, left: AID, right: VC) extends Instruction

final case class BinOpVTree(op: BinOpV, left: AID, right: AID) extends Instruction

case object Nop extends Instruction

case object Read extends Instruction

final case class Write(value: AID) extends Instruction

final case class CallDir(fn: LabelID, args: List[AID]) extends Instruction

final case class Save(reg: AReg, memory: AVar) extends Instruction

final case class Restore(memory: AVar) extends Instruction
