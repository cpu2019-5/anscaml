package net.akouryy.anscaml

object Logger {
  def log(tag: String, message: String): Unit = {
    val time = (System.nanoTime - AnsCaml.startTimeNano) * 1E-9
    println(f"$time%6.3f [$tag] $message")
  }
}
