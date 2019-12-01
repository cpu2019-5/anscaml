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

    println("[Lexer] start")

    val tokens = syntax.Lexer.lex(code)

    println("[Parser] start")

    val ast = syntax.Parser.parse(tokens)

    println("[Typing] start")

    val astTyped = typ.Typing.solve(ast)

    println("[KNorm Converter] start")

    val kn = knorm.Converter(astTyped)

    val alpha = knorm.Alpha(kn)

    val ko = knorm.optimize.Optimizer(config.optimizationCount, alpha)

    val dbg = new java.io.PrintWriter("../dbg.txt")
    pprinter.tokenize(ko, height = 100000).foreach(x => dbg.write(x.toString))
    dbg.close()

    println(s"time: ${System.nanoTime() - startTime}ns")
  }
}
