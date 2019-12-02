package net.akouryy.anscaml

import scala.io.Source
import base._

object AnsCaml {
  val VERSION = "2.0"

  var config: CommandParser.Config = _

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()

    config = CommandParser.parse(args).getOrElse(???)

    val code = {
      val libCode = if (config.doPrependStandardLibrary) Source.fromResource("lib.tig.ml").mkString else ""
      (libCode +: config.inputFiles.map(Source.fromFile(_).mkString)).mkString(";\n")
    }

    val tokens = syntax.Lexer.lex(code)

    val ast = syntax.Parser.parse(tokens)

    val astTyped = typ.Typing.solve(ast)

    val kn = knorm.Converter(astTyped)

    val alpha = knorm.Alpha(kn)

    val ko = knorm.optimize.Optimizer(config.optimizationCount, alpha)

    val cl = new knorm.Closer()(ko)

    val dbg = new java.io.PrintWriter("../dbg.txt")
    PPrinter.writeTo(dbg, cl)
    dbg.close()

    val t = System.nanoTime() - startTime
    println(s"time: ${t / 1e9}s")
  }
}
