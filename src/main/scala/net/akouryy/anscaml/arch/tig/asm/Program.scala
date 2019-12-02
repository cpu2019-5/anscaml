package net.akouryy.anscaml
package arch.tig.asm

import base.{ID, LabelID}
import syntax.CmpOp

import scala.collection.mutable

final case class Program(gcSize: Int, tyEnv: Map[AVar, Ty], functions: List[FDef])

final case class FDef(name: LabelID, args: List[ID], body: Chart, typ: Fn)

final case class BlockIndex(index: Int)

object BlockIndex {
  private[this] var cnt = -1

  def generate(): BlockIndex = {
    cnt += 1
    BlockIndex(cnt)
  }
}

final case class JumpIndex(index: Int)

object JumpIndex {
  private[this] var cnt = -1

  def generate(): JumpIndex = {
    cnt += 1
    JumpIndex(cnt)
  }
}

/** フローチャート。whileやfor構文がないためDAG */
final class Chart {
  val blocks: mutable.Map[BlockIndex, Block] = mutable.LinkedHashMap()
  val jumps: mutable.Map[JumpIndex, Jump] = mutable.LinkedHashMap()
}

/** 基本ブロック */
final case class Block(i: BlockIndex, lines: List[Line], input: JumpIndex, output: JumpIndex)

final case class Line(dest: AID, inst: Instruction)

sealed trait Jump

final case class StartFun(i: JumpIndex, output: BlockIndex) extends Jump

final case class Return(i: JumpIndex, value: AID, input: BlockIndex) extends Jump

final case class Condition(
  i: JumpIndex,
  op: CmpOp, left: AID, right: AID,
  input: BlockIndex, trueOutput: BlockIndex, falseOutput: BlockIndex
) extends Jump

final case class Merge(
  i: JumpIndex, inputs: List[(AID, BlockIndex)], outputID: AID, output: BlockIndex,
) extends Jump
