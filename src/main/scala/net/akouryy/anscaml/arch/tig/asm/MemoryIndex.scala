package net.akouryy.anscaml.arch.tig.asm

sealed trait MemoryIndex {
  def ~(that: MemoryIndex): Boolean = (this, that) match {
    case (MIUnknown, _) | (_, MIUnknown) => true
    case (MIArray(i), MIArray(j)) => i.forall(i => j.forall(i == _))
    case (MITuple(i), MITuple(j)) => i.forall(i => j.forall(i == _))
    case _ => false
  }

  def !~(that: MemoryIndex): Boolean = !(this ~ that)
}

final case class MIArray(i: Option[Int] = None) extends MemoryIndex

object MIArray {
  def apply(i: Int): MIArray = MIArray(Some(i))
}

final case class MITuple(i: Option[Int] = None) extends MemoryIndex

object MITuple {
  def apply(i: Int): MITuple = MITuple(Some(i))
}

case object MIUnknown extends MemoryIndex
