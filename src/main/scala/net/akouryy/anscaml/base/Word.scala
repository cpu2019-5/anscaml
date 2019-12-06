package net.akouryy.anscaml.base

final case class Word private(int: Int, float: Float) {
  @annotation.unused
  private[this] def copy(int: Int = this.int, float: Float = this.float): Word = ???

  override def toString: String = s"$int:$float"
}

object Word {
  def fromInt(int: Int) = new Word(int, java.lang.Float.intBitsToFloat(int))

  def fromFloat(float: Float) = new Word(java.lang.Float.floatToRawIntBits(float), float)

  private[this] def apply(v: Int): Word = ???
}
