package net.akouryy.anscaml

package object base {

  implicit class RichList[A](val list: List[A]) extends AnyVal {
    def zipMap[B, C](that: IterableOnce[B])(fn: (A, B) => C): List[C] =
      list.zip(that).map(fn.tupled)
  }

}
