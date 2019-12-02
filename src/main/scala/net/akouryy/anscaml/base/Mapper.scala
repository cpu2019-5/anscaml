package net.akouryy.anscaml.base

final case class Mapper[A, B, C](map: (A => B) => C) {
  val flatMap: map.type = map
  val andThen: map.type = map

  def withFilter(@annotation.unused _x: A => Boolean): this.type = this
}
