package net.akouryy.anscaml
package arch.tig.asm

import scala.collection.mutable

final case class FDef(name: String, args: List[XID], body: Chart, typ: Fn, info: FDefInfo) {
  def blocks: mutable.SortedMap[BlockIndex, Block] = body.blocks

  def jumps: mutable.SortedMap[JumpIndex, Jump] = body.jumps
}

final case class FDefInfo(isLeaf: Boolean)
