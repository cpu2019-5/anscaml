package net.akouryy.anscaml

import scala.collection.mutable

package object base {

  implicit class RichAny[A](val implicitOriginal: A) extends AnyVal {
    def |>[B](f: A => B): B = f(implicitOriginal)
  }

  implicit class RichList[A](val implicitOriginal: List[A]) extends AnyVal {
    def mkCommaString: String = implicitOriginal.mkString(", ")

    def zipStrict[B, C](that: Iterable[B]): List[(A, B)] = {
      require(implicitOriginal.sizeCompare(that) == 0, (implicitOriginal, that))
      implicitOriginal zip that
    }

    def zipMap[B, C](that: Iterable[B])(fn: (A, B) => C): List[C] = {
      zipStrict(that).map(fn.tupled)
    }

    def mapInReversedOrder[B](fn: A => B): List[B] = {
      implicitOriginal.reverseIterator.map(fn).toList.reverse
    }

    def foldLeftNonempty(fn: (A, A) => A): A = implicitOriginal.tail.foldLeft(implicitOriginal.head)(fn)
  }

  implicit class RichOption[A](val implicitOriginal: Option[A]) extends AnyVal {
    /** `fold` with swapped arguments */
    def foldF[B](f: A => B, ifEmpty: => B): B = implicitOriginal.fold(ifEmpty)(f)
  }

  implicit class RichString(val implicitOriginal: String) extends AnyVal {
    /** strictly typed `+` */
    def +!(that: String): String = implicitOriginal + that
  }

  implicit class RichMutMap[K, V](val implicitOriginal: mutable.Map[K, V]) extends AnyVal {
    def updateByGet(to: K, from: K): Unit =
      implicitOriginal.get(from).foreach(implicitOriginal(to) = _)
  }

  def !!!!(a: Any): Nothing = throw new IllegalArgumentException(a.toString)

  def ????(a: Any): Nothing = throw new NotImplementedError(a.toString)

}
