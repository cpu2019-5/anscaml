package net.akouryy.anscaml

import java.io.File
import scopt.OParser

object CommandParser {

  final case class Config(
    asmIn: Option[File] = None,
    asmOut: Option[File] = None,
    doEmitComment: Boolean = true,
    doPrependStandardLibrary: Boolean = true,
    inputFiles: Seq[File] = Seq(),
    inlineLimit: Int = 0,
    outputFile: File = new File("."),
    memorySizeLog2: Int = 19,
    optimizationCount: Int = 100,
    kcIn: Option[File] = None,
    kcOut: Option[File] = None,
    knIn: Option[File] = None,
    knOut: Option[File] = None,
    // verbosity: Verbosity = Verbosity.Stats,
  )

  private[this] val cmdParser = {
    val builder = OParser.builder[Config]
    import builder._

    OParser.sequence(
      programName("anscaml"),
      head("anscaml", AnsCaml.VERSION),

      opt[Unit]('c', "comment")
        .action((_, c) => c.copy(doEmitComment = true))
        .text("Emit comments in assembly"),
      opt[Unit]('C', "no-comment")
        .action((_, c) => c.copy(doEmitComment = false))
        .text("Do not emit comments in assembly"),
      opt[Int]('i', "inline")
        .action((x, c) => c.copy(inlineLimit = x))
        .text("Set maximum size of functions inlined"),
      opt[Unit]('L', "no-stdlib")
        .action((_, c) => c.copy(doPrependStandardLibrary = false))
        .text("Do not prepend standard library source"),
      opt[Int]('m', "memory")
        .action((x, c) => c.copy(memorySizeLog2 = x))
        .text("Set memory size to 2^n"),
      opt[Int]('o', "optimize")
        .action((x, c) => c.copy(optimizationCount = x))
        .text("Set maximum iteration counts of optimizations"),
      opt[File]("kni")
        .action((x, c) => c.copy(knIn = Some(x)))
        .text("Input file when interpreting intermediate representation `KNorm`"),
      opt[File]("kno")
        .action((x, c) => c.copy(knOut = Some(x)))
        .text("Output file when interpreting intermediate representation `KNorm`"),
      opt[File]("kci")
        .action((x, c) => c.copy(kcIn = Some(x)))
        .text("Input file when interpreting intermediate representation `KClosed`"),
      opt[File]("kco")
        .action((x, c) => c.copy(kcOut = Some(x)))
        .text("Output file when interpreting intermediate representation `KClosed`"),
      opt[File]("asmi")
        .action((x, c) => c.copy(asmIn = Some(x)))
        .text("Input file when interpreting intermediate representation `asm`"),
      opt[File]("asmo")
        .action((x, c) => c.copy(asmOut = Some(x)))
        .text("Output file when interpreting intermediate representation `asm`"),
      arg[File]("<output>")
        .action((x, c) => c.copy(outputFile = x))
        .text("output file name"),
      arg[File]("<input>...")
        .unbounded
        .action((x, c) => c.copy(inputFiles = c.inputFiles :+ x))
        .text("input file names"),
      checkConfig { c =>
        if (c.knIn.isDefined != c.knOut.isDefined) {
          failure("specify both `kni` and `kno`.")
        } else if (c.kcIn.isDefined != c.kcOut.isDefined) {
          failure("specify both `kci` and `kco`.")
        } else if (c.asmIn.isDefined != c.asmOut.isDefined) {
          failure("specify both `asmi` and `asmo`.")
        } else success
      }
    )
  }

  def parse(args: Array[String]): Option[Config] = {
    OParser.parse(cmdParser, args, Config())
  }
}
