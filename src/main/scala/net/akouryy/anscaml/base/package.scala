package net.akouryy.anscaml

import scala.collection.mutable

package object base {

  implicit class RichAny[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }

  implicit class RichList[A](val list: List[A]) extends AnyVal {
    def mkCommaString: String = list.mkString(", ")

    def zipStrict[B, C](that: Iterable[B]): List[(A, B)] = {
      require(list.sizeCompare(that) == 0, (list, that))
      list zip that
    }

    def zipMap[B, C](that: Iterable[B])(fn: (A, B) => C): List[C] = {
      zipStrict(that).map(fn.tupled)
    }

    def mapInReversedOrder[B](fn: A => B): List[B] = {
      list.reverseIterator.map(fn).toList.reverse
    }

    def foldLeftNonempty(fn: (A, A) => A): A = list.tail.foldLeft(list.head)(fn)
  }

  implicit class RichOption[A](val option: Option[A]) extends AnyVal {
    /** `fold` with swapped arguments */
    def foldF[B](f: A => B, ifEmpty: => B): B = option.fold(ifEmpty)(f)
  }

  implicit class RichString(val string: String) extends AnyVal {
    /** strictly typed `+` */
    def +!(that: String): String = string + that
  }

  implicit class RichMutMap[K, V](val map: mutable.Map[K, V]) extends AnyVal {
    def updateByGet(to: K, from: K): Unit = map.get(from).foreach(map(to) = _)
  }

  def !!!!(a: Any): Nothing = throw new IllegalArgumentException(a.toString)

  def ????(a: Any): Nothing = throw new NotImplementedError(a.toString)

}
