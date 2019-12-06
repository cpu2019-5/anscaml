package net.akouryy.anscaml.base

final case class Word private(int: Int, float: Float) {

  @annotation.unused
  private[this] def copy(_int: Int, _float: Float): Word = ???

  def isGoodFloat: Boolean =
    int == 0 || java.lang.Float.MIN_NORMAL <= float.abs && float.abs <= Float.MaxValue

  override def toString: String =
    if (isGoodFloat) {
      s"$int:$float"
    } else {
      s"$int:?"
    }
}

object Word {
  def fromInt(int: Int) = new Word(int, java.lang.Float.intBitsToFloat(int))

  def fromFloat(float: Float) = new Word(java.lang.Float.floatToRawIntBits(float), float)

  private[this] def apply(v: Int): Word = ???
}
