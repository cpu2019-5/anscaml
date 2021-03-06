package net.akouryy.anscaml
package arch.tig.asm

import swarm.SwarmIndex

sealed trait MemoryIndex {
  /** @return 同じ領域を指しうるならtrue */
  def ~(that: MemoryIndex): Boolean = (this, that) match {
    case (MITuple, _) | (_, MITuple) =>
      // タプル要素をStoreするときそれは新たなタプルなので既存のタプルとは干渉しない
      false
    case (MIUnknown, _) | (_, MIUnknown) => true
    case (MIArray(sw, i), MIArray(sx, j)) => sw == sx && i.forall(i => j.forall(i == _))
    case _ => false
  }

  def !~(that: MemoryIndex): Boolean = !(this ~ that)
}

final case class MIArray(sw: SwarmIndex, i: Option[Int] = None) extends MemoryIndex

object MIArray {
  def apply(sw: SwarmIndex, i: Int): MIArray = MIArray(sw, Some(i))
}

case object MITuple extends MemoryIndex

case object MIUnknown extends MemoryIndex
