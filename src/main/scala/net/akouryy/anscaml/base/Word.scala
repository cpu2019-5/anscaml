package net.akouryy.anscaml.base

import scala.annotation.unused

final case class Word private(int: Int) {
  val float: Float = java.lang.Float.intBitsToFloat(int)

  @unused
  private[this] def copy(_int: Int): Word = ???

  def isGoodFloat: Boolean =
    int == 0 || java.lang.Float.MIN_NORMAL <= float.abs && float.abs <= Float.MaxValue

  override def toString: String =
    if (int == 0) {
      "0"
    } else if (isGoodFloat) {
      s"$int:$float"
    } else {
      s"$int:"
    }
}

object Word {
  def fromInt(int: Int) = new Word(int)

  def fromFloat(float: Float): Word = {
    if (float == 0.0F) {
      new Word(0)
    } else {
      new Word(java.lang.Float.floatToRawIntBits(float))
    }
  }

  @unused private[this] def apply(v: Int): Word = ???
}
