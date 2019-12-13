package net.akouryy.anscaml
package arch.tig.asm

import base._

import scala.collection.mutable

final case class Program(gcSize: Int, tyEnv: Map[XVar, Ty], functions: List[FDef])

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

final case class Line(comment: Comment, dest: XID, inst: Instruction)

sealed trait Jump {
  val comment: Comment
  val i: JumpIndex
}

final case class StartFun(comment: Comment, i: JumpIndex, output: BlockIndex) extends Jump

final case class Return(comment: Comment, i: JumpIndex, value: XID, input: BlockIndex) extends Jump

final case class Branch(
  comment: Comment, i: JumpIndex,
  cond: Branch.Cond, input: BlockIndex, tru: BlockIndex, fls: BlockIndex,
) extends Jump

object Branch {

  sealed trait Cond {
    val opBase: CmpOp
    val left: XID
    val rightVC: VC

    def mapL(leftFn: XID => XID): Cond

    def mapLR(leftFn: XID => XID)(rightFnVC: VC => VC, rightFnV: XID => XID): Cond
  }

  object Cond {
    def unapply(c: Cond): Option[(CmpOp, XID, VC)] = Some(c.opBase, c.left, c.rightVC)
  }

  final case class CondVC(op: CmpOpVC, left: XID, right: VC) extends Cond {
    override val opBase: CmpOp = op
    override val rightVC: VC = right

    override def mapL(leftFn: XID => XID): CondVC = copy(left = leftFn(left))

    override def mapLR(leftFn: XID => XID)(rightFnVC: VC => VC, rightFnV: XID => XID): CondVC =
      copy(left = leftFn(left), right = rightFnVC(right))
  }

  final case class CondV(op: CmpOpV, left: XID, right: XID) extends Cond {
    override val opBase: CmpOp = op
    override val rightVC: VC = V(right)

    override def mapL(leftFn: XID => XID): CondV = copy(left = leftFn(left))

    override def mapLR(leftFn: XID => XID)(rightFnVC: VC => VC, rightFnV: XID => XID): CondV =
      copy(left = leftFn(left), right = rightFnV(right))
  }

}

final case class Merge(
  comment: Comment, i: JumpIndex,
  inputs: List[(XID, BlockIndex)], outputID: XID, output: BlockIndex,
) extends Jump
