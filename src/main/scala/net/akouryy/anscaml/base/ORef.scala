package net.akouryy.anscaml.base

final class ORef[T](var contents: T) {
  def :=(t: T): Unit = contents = t
}

object ORef {
  /**
    * 返り値を `T` 型にすると `ORef[Option[Hoge]]` がうまく扱えなかったので `Option[T]` を使用している
    */
  def unapply[T](o: ORef[T]): Option[T] = Some(o.contents)
}
