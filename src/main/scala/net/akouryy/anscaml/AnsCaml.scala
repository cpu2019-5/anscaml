package net.akouryy.anscaml

import java.io.FileInputStream

import base._
import net.akouryy.anscaml.arch.tig.emit.Emitter

import scala.io.Source

object AnsCaml {
  val VERSION = "2.0"

  var config: CommandParser.Config = _

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()

    config = CommandParser.parse(args).getOrElse(???)

    val code = "let _ = " + {
      val libCode = if (config.doPrependStandardLibrary) Source.fromResource("lib.tig.ml").mkString else ""
      (libCode +: config.inputFiles.map(Source.fromFile(_).mkString)).mkString(";\n")
    } + " in ()"

    val tokens = syntax.Lexer.lex(code)

    val ast = syntax.Parser.parse(tokens)

    val astTyped = typ.Typing.solve(ast)

    val kn = knorm.Converter(astTyped)

    val alpha = knorm.Alpha(kn)

    val ko = knorm.optimize.Optimizer(config.optimizationCount, alpha)

    val cl = new knorm.Closer()(ko)

    config.runKC match {
      case Some(file) =>
        val kcOut = new java.io.PrintWriter("../temp/kc.out")
        kcOut.write(new knorm.debug.KCInterpreter()(cl, new FileInputStream(file)))
        kcOut.close()
      case None => // nop
    }

    val asm = new arch.tig.Specializer()(cl)

    val rawDot = new java.io.PrintWriter("../temp/raw.dot")
    rawDot.write(new arch.tig.GraphDrawer()(asm))
    rawDot.close()

    arch.tig.optimize.Optimizer(config.optimizationCount, asm)

    val dot = new java.io.PrintWriter("../temp/dbg.dot")
    dot.write(new arch.tig.GraphDrawer()(asm))
    dot.close()

    val dbg = new java.io.PrintWriter("../temp/dbg.txt")
    PPrinter.writeTo(dbg, arch.tig.analyze.Liveness.analyzeProgram(asm).toList.sortBy(_._1))
    dbg.close()

    val reg = new arch.tig.RegisterAllocator()(asm, arch.tig.analyze.Liveness.analyzeProgram(asm))

    val lo = arch.tig.emit.LastOptimizer(reg)

    val rDot = new java.io.PrintWriter("../temp/reg.dot")
    rDot.write(new arch.tig.GraphDrawer()(lo))
    rDot.close()

    val out = new java.io.PrintWriter(config.outputFile)
    new Emitter(lo).writeTo(out)
    out.close()

    val t = System.nanoTime() - startTime
    println(s"time: ${t / 1e9}s")
  }
}
