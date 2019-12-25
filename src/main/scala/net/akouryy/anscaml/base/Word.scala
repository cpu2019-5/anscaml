package net.akouryy.anscaml.base

final case class Word(int: Int) {
  val float: Float = java.lang.Float.intBitsToFloat(int)

  def isGoodFloat: Boolean =
    int == 0 || java.lang.Float.MIN_NORMAL <= float.abs && float.abs <= Float.MaxValue

  override def toString: String =
    if (int != 0 && isGoodFloat) {
      s"$int:$float"
    } else {
      s"$int:"
    }

  def unary_~ = Word(~int)
}

object Word {
  def fromFloat(float: Float): Word = {
    if (float == 0.0F) {
      new Word(0)
    } else {
      new Word(java.lang.Float.floatToRawIntBits(float))
    }
  }

  object WithFloat {
    def unapply(w: Word): Option[Float] = Some(w.float)
  }

}
