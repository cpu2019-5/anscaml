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
final case class Block(i: BlockIndex, lines: List[Line], input: JumpIndex, output: JumpIndex) {
  assert(input < output)

  def :+(line: Line): Block = copy(lines = lines :+ line)
}

final case class Line(comment: Comment, dest: XID, inst: Instruction)

sealed trait Jump {
  val comment: Comment
  val i: JumpIndex

  final def convertInput(from: BlockIndex, to: BlockIndex): Jump = this match {
    case j @ Return(_, _, _, _, `from`) => j.copy(input = to)
    case j @ Branch(_, _, _, `from`, _, _) => j.copy(input = to)
    case j @ Merge(_, _, inputs, _, _) if inputs.exists(_.bi == from) =>
      j.copy(inputs = inputs.map(_.mapBI(bi => if (bi == from) to else bi)))
    case _ => !!!!(this, from, to)
  }

  /**
    * @return The indices of `blocks`, such that each Block whose input is `this` is guaranteed
    *         to be run right after one of `blocks`.
    */
  final def directInputBIs(c: Chart): List[BlockIndex] = this match {
    case _: StartFun => Nil
    case Return(_, _, _, _, input) => List(input)
    case Branch(_, _, _, input, _, _) => List(input)
    case Merge(_, _, inputs, _, _) => inputs.map(_.bi)
    case ForLoopTop(_, _, _, _, _, input, flb, _, _) =>
      List(input, c.jumps(flb).asInstanceOf[ForLoopBottom].input)
    case _: ForLoopBottom => !!!!(this)
  }
}

final case class StartFun(comment: Comment, i: JumpIndex, output: BlockIndex) extends Jump

final case class Return(
  comment: Comment, i: JumpIndex, value: XID, address: Option[XID], input: BlockIndex,
) extends Jump

final case class Branch(
  comment: Comment, i: JumpIndex,
  cond: Branch.Cond, input: BlockIndex, tru: BlockIndex, fls: BlockIndex,
) extends Jump {
  assert(input < tru && input < fls)
}

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
  inputs: List[MergeInput], outputID: XID, output: BlockIndex,
) extends Jump {
  assert(inputs.forall(_.bi < output))
}

final case class MergeInput(bi: BlockIndex, xid: XID) {
  def toPair: (BlockIndex, XID) = (bi, xid)

  def mapBI(fn: BlockIndex => BlockIndex): MergeInput = MergeInput(fn(bi), xid)

  def mapXID(fn: XID => XID): MergeInput = MergeInput(bi, fn(xid))
}

/**
  * @see net.akouryy.anscaml.knorm.KNorm.ForCmp
  */
final case class ForLoopTop(
  comment: Comment, i: JumpIndex, cond: Branch.Cond, negated: Boolean, merges: List[ForLoopVar],
  input: BlockIndex, loopBottom: JumpIndex, body: BlockIndex, kont: BlockIndex,
) extends Jump {

  val loopVars: List[XVar] = merges.flatMap(_.loop.asXVar)

  assert(merges.flatMap(_.loop.asXReg).forall(!XReg.toConstants.contains(_)))

  assert(loopVars.isEmpty ||
         (cond.left.asXVar ++ cond.rightVC.asVXVar).count(loopVars contains _) == 1)
}

final case class ForLoopBottom(
  comment: Comment, i: JumpIndex, input: BlockIndex, loopTop: JumpIndex,
) extends Jump

final case class ForLoopVar(in: XID, upd: XID, loop: XID)
