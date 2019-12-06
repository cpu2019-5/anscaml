package net.akouryy.anscaml
package arch.tig.asm

import base.{ID, LabelID}

import scala.collection.mutable

final case class Program(gcSize: Int, tyEnv: Map[AVar, Ty], functions: List[FDef])

final case class FDef(name: LabelID, args: List[AID], body: Chart, typ: Fn)

final case class BlockIndex(indices: List[Int]) extends Ordered[BlockIndex] {
  override def toString: String = s"Block$indexString"

  def indexString: String = indices.mkString("_")

  import Ordering.Implicits._

  def compare(that: BlockIndex): Int =
    implicitly[Ordering[List[Int]]].compare(indices, that.indices)
}

object BlockIndex {
  private[this] var cnt = -1

  def generate(prefix: BlockIndex = BlockIndex(Nil)): BlockIndex = {
    cnt += 1
    BlockIndex(prefix.indices :+ cnt) // O(n)
  }
}

final case class JumpIndex(indices: List[Int]) extends Ordered[JumpIndex] {
  override def toString: String = s"Jump$indexString"

  def indexString: String = indices.mkString("_")

  import Ordering.Implicits._

  def compare(that: JumpIndex): Int = implicitly[Ordering[List[Int]]].compare(indices, that.indices)

}

object JumpIndex {
  private[this] var cnt = -1

  def generate(prefix: JumpIndex = JumpIndex(Nil)): JumpIndex = {
    cnt += 1
    JumpIndex(prefix.indices :+ cnt) // O(n)
  }
}

/** フローチャート。whileやfor構文がないためDAG */
final class Chart {
  val blocks: mutable.SortedMap[BlockIndex, Block] = mutable.SortedMap[BlockIndex, Block]()
  val jumps: mutable.SortedMap[JumpIndex, Jump] = mutable.SortedMap[JumpIndex, Jump]()
}

/** 基本ブロック */
final case class Block(i: BlockIndex, lines: List[Line], input: JumpIndex, output: JumpIndex)

final case class Line(dest: AID, inst: Instruction)

sealed trait Jump {
  val i: JumpIndex
}

final case class StartFun(i: JumpIndex, output: BlockIndex) extends Jump

final case class Return(i: JumpIndex, value: AID, input: BlockIndex) extends Jump

final case class Condition(
  i: JumpIndex,
  op: CmpOp, left: AID, right: VC,
  input: BlockIndex, tru: BlockIndex, fls: BlockIndex,
) extends Jump

final case class Merge(
  i: JumpIndex,
  /*private var */ inputs: List[(AID, BlockIndex)],
  outputID: AID,
  output: BlockIndex,
) extends Jump {
  /*val inputs: List[(AID, BlockIndex)] =
    if (outputID == AReg.REG_DUMMY)
      _inputs.map { case (_, output) => (AReg.REG_DUMMY, output) }
    else _inputs

  _inputs = inputs*/
}
