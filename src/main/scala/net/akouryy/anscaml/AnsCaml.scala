package net.akouryy.anscaml

import base._

import scala.io.Source

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

    val dbg = new java.io.PrintWriter("../dbg.txt")
    PPrinter.writeTo(dbg, kn)
    dbg.close()

    val alpha = knorm.Alpha(kn)

    val ko = knorm.optimize.Optimizer(config.optimizationCount, alpha)

    val cl = new knorm.Closer()(ko)

    val asm = new arch.tig.Specializer()(cl)

    val rawDot = new java.io.PrintWriter("../raw.dot")
    rawDot.write(new arch.tig.GraphDrawer()(asm))
    rawDot.close()

    arch.tig.optimize.Optimizer(config.optimizationCount, asm)

    val dot = new java.io.PrintWriter("../dbg.dot")
    dot.write(new arch.tig.GraphDrawer()(asm))
    dot.close()

    val t = System.nanoTime() - startTime
    println(s"time: ${t / 1e9}s")
  }
}
