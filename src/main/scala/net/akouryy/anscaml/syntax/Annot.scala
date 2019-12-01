package net.akouryy.anscaml.syntax

object Annot {
  val NoInline: String = "no_inline"
  val TypFold: String = "typ_fold"

  val Annotations: Set[String] = Set(NoInline, TypFold)
}
