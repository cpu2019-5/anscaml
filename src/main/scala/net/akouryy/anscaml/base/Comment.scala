package net.akouryy.anscaml.base

sealed trait Comment {
  def +:(str: String): Comment = this match {
    case NC => CM(str)
    case CM(msg) => CM(s"$str; $msg")
  }

  def :+(str: String): Comment = this match {
    case NC => CM(str)
    case CM(msg) => CM(s"$msg; $str")
  }

  def +(c2: Comment): Comment = this match {
    case NC => c2
    case CM(msg) => msg +: c2
  }
}

case object NC extends Comment

case class CM(msg: String) extends Comment
