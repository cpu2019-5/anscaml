package net.akouryy.anscaml

import java.io.{FileInputStream, FileOutputStream}

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

    (config.knIn, config.knOut) match {
      case (Some(in), Some(out)) =>
        val knIn = new FileInputStream(in)
        val knOut = new FileOutputStream(out)
        new knorm.debug.KNInterpreter()(kn, knIn, knOut)
        knOut.close()
        knIn.close()
      case _ => // nop
    }

    val alpha = knorm.Alpha(kn)

    val ko = knorm.optimize.Optimizer(config.optimizationCount, alpha)

    val cl = new knorm.Closer()(ko)

    (config.kcIn, config.kcOut) match {
      case (Some(in), Some(out)) =>
        val kcIn = new FileInputStream(in)
        val kcOut = new FileOutputStream(out)
        new knorm.debug.KCInterpreter()(cl, kcIn, kcOut)
        kcOut.close()
        kcIn.close()
      case _ => // nop
    }

    val asm = new arch.tig.Specializer()(cl)

    val rawDot = new java.io.PrintWriter("../temp/raw.dot")
    rawDot.write(new arch.tig.GraphDrawer()(asm))
    rawDot.close()

    println((config.asmIn, config.asmOut))
    (config.asmIn, config.asmOut) match {
      case (Some(in), Some(out)) =>
        val asmIn = new FileInputStream(in)
        val asmOut = new FileOutputStream(out)
        new arch.tig.asm.AsmInterpreter()(asm, asmIn, asmOut)
        asmOut.close()
        asmIn.close()
      case _ => // nop
    }

    arch.tig.optimize.Optimizer(config.optimizationCount, asm)

    val dot = new java.io.PrintWriter("../temp/dbg.dot")
    dot.write(new arch.tig.GraphDrawer()(asm))
    dot.close()

    val dbg = new java.io.PrintWriter("../temp/dbg.txt")
    PPrinter.writeTo(dbg, astTyped)
    //                    arch.tig.analyze.Liveness.analyzeProgram(asm).toList.sortBy(_._1))
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
