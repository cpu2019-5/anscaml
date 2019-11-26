package net.akouryy.anscaml

import scala.io.Source

import base._

object AnsCaml {
  val VERSION = "2.0"

  def main(args: Array[String]): Unit = {
    val config = CommandParser.parse(args).getOrElse(???)

    val code = {
      val libCode = if(config.doPrependStandardLibrary) Source.fromResource("lib.tig.ml").mkString else ""
      (libCode +: config.inputFiles.map(Source.fromFile(_).mkString)).mkString(";\n")
    }

    val tokens = syntax.Lexer.lex(code)

    val ast = syntax.Parser.parse(tokens)

    pprinter.pprintln(ast)
  }
}
