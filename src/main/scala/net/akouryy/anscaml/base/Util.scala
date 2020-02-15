package net.akouryy.anscaml.base

object Util {
  def log2(n: Int): Option[Int] = {
    if (n < 1) return None
    var i = 0
    var x = n
    while (x > 1) {
      if (x % 2 == 1) return None
      i += 1
      x /= 2
    }
    Some(i)
  }

  def log2Ceil(n: Int): Int = {
    if (n < 1) return 0
    var i = 1
    var x = n - 1
    while (x > 1) {
      i += 1
      x /= 2
    }
    i
  }

  object The {
    /** https://github.com/scala/bug/issues/900 */
    def unapply[A](a: A): Option[A] = Some(a)
  }

}
