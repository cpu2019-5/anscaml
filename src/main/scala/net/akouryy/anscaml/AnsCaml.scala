package net.akouryy.anscaml

import java.io.{FileInputStream, FileOutputStream}

import scala.io.Source
import scala.util.Using

object AnsCaml {
  val VERSION = "2.0"

  var config: CommandParser.Config = _

  var startTimeNano: Long = _

  def main(args: Array[String]): Unit = {
    startTimeNano = System.nanoTime()

    config = CommandParser.parse(args).getOrElse(???)

    val code = "let _ = " + {
      val libCode =
        if (config.doPrependStandardLibrary)
          Using.resource(Source.fromResource("lib.tig.ml"))(_.mkString)
        else ""
      val userCodes = config.inputFiles.map(f => Using.resource(Source.fromFile(f))(_.mkString))
      (libCode +: userCodes).mkString(";\n")
    } + " in ()"

    val tokens = syntax.Lexer.lex(code)

    val ast = syntax.Parser.parse(tokens)

    val astTyped = typ.Typing.solve(ast)

    val kn = knorm.Converter(astTyped)

    (config.knIn, config.knOut) match {
      case (Some(in), Some(out)) =>
        Using.resources(new FileInputStream(in), new FileOutputStream(out)) {
          new knorm.debug.KNInterpreter()(kn, _, _)
        }
      case _ => // nop
    }

    val alpha = knorm.Alpha(kn)

    // PPrinter.pprintln(alpha)

    val ko = knorm.optimize.Optimizer(config.optimizationCount, alpha)
    // util.Using(new java.io.PrintWriter("../temp/b.txt"))(base.PPrinter.writeTo(_, ko))

    val sw = new knorm.SwarmAnalyzer()(ko)

    val cl = new knorm.Closer()(ko)

    // PPrinter.pprintln(cl)

    (config.kcIn, config.kcOut) match {
      case (Some(in), Some(out)) =>
        val kcIn = new FileInputStream(in)
        val kcOut = new FileOutputStream(out)
        new knorm.debug.KCInterpreter()(cl, kcIn, kcOut)
        kcOut.close()
        kcIn.close()
      case _ => // nop
    }

    val (asm, _) = new arch.tig.Specializer()(cl, sw)

    (config.asmIn, config.asmOut) match {
      case (Some(in), Some(out)) =>
        Using.resources(new FileInputStream(in), new FileOutputStream(out)) {
          new arch.tig.asm.AsmInterpreter()(asm, _, _)
        }
      case _ => // nop
    }

    if (config.xGenerateAsmGraphs) {
      for ((fn, graph) <- new arch.tig.GraphDrawer()(asm)) {
        Using.resource(new java.io.PrintWriter(s"../temp/graph/raw-$fn.dot")) {
          _.write(graph)
        }
      }
    }

    try {
      arch.tig.optimize.Optimizer(config.optimizationCount, asm)
    } finally {
      if (config.xGenerateAsmGraphs) {
        for ((fn, graph) <- new arch.tig.GraphDrawer()(asm)) {
          Using.resource(new java.io.PrintWriter(s"../temp/graph/dbg-$fn.dot")) {
            _.write(graph)
          }
        }
      }
    }

    val reg = new arch.tig.RegisterAllocator()(asm)

    val lo = arch.tig.emit.LastOptimizer(reg)

    if (config.xGenerateAsmGraphs) {
      for ((fn, graph) <- new arch.tig.GraphDrawer()(lo)) {
        Using.resource(new java.io.PrintWriter(s"../temp/graph/reg-$fn.dot")) {
          _.write(graph)
        }
      }
    }

    Using.resource(new java.io.PrintWriter(config.outputFile)) {
      new arch.tig.emit.Emitter(lo).writeTo(_)
    }

    val t = System.nanoTime() - startTimeNano
    Logger.log("AC", s"time: ${t / 1e9}s")
  }
}
