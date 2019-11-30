package net.akouryy.anscaml.base

sealed trait Comment {
  def +:(str: String): Comment = this match {
    case NoComment => Commented(str)
    case Commented(msg) => Commented(s"$str; $msg")
  }

  def :+(str: String): Comment = this match {
    case NoComment => Commented(str)
    case Commented(msg) => Commented(s"$msg; $str")
  }

  def +(c2: Comment): Comment = this match {
    case NoComment => c2
    case Commented(msg) => msg +: c2
  }
}

case object NoComment extends Comment

case class Commented(msg: String) extends Comment
