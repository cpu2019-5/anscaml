package net.akouryy.anscaml

package object base {

  implicit class RichList[A](val list: List[A]) extends AnyVal {
    def mkCommaString: String = list.mkString(", ")

    def zipStrict[B, C](that: Iterable[B]): List[(A, B)] = {
      require(list.sizeCompare(that) == 0, (list, that))
      list zip that
    }

    def zipMap[B, C](that: Iterable[B])(fn: (A, B) => C): List[C] = {
      zipStrict(that).map(fn.tupled)
    }
  }

  implicit class RichOption[A](val option: Option[A]) extends AnyVal {
    /** `fold` with swapped arguments */
    def foldF[B](f: A => B, ifEmpty: => B): B = option.fold(ifEmpty)(f)
  }

  def ????(a: Any): Nothing = throw new IllegalArgumentException(a.toString)

}
