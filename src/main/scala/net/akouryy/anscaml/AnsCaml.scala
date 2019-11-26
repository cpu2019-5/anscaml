package net.akouryy.anscaml

object AnsCaml {
  val VERSION = "2.0"

  def main(args: Array[String]) = {
    println(CommandParser.parse(args))
  }
}
