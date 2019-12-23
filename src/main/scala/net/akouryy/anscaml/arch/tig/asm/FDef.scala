package net.akouryy.anscaml
package arch.tig.asm

import scala.collection.{immutable, mutable}

final case class FDef(name: String, args: List[XID], body: Chart, typ: Fn, info: FDefInfo) {
  def blocks: mutable.SortedMap[BlockIndex, Block] = body.blocks

  def jumps: mutable.SortedMap[JumpIndex, Jump] = body.jumps

  def apply(bi: BlockIndex): Block = blocks(bi)

  def apply(ji: JumpIndex): Jump = jumps(ji)

  def get(bi: BlockIndex): Option[Block] = blocks.get(bi)

  def get(ji: JumpIndex): Option[Jump] = jumps.get(ji)

  def update(bi: BlockIndex, b: Block): Unit = blocks(bi) = b

  def update(ji: JumpIndex, j: Jump): Unit = jumps(ji) = j

}

/**
  * @param isLeaf 非末尾の関数呼び出しが存在しないなら真
  */
final case class FDefInfo(isLeaf: Boolean, safeRegs: immutable.SortedSet[XReg])
