package net.akouryy.anscaml
package arch.tig.asm

import java.io.{FileInputStream, FileOutputStream}

import base._

import scala.collection.mutable
import scala.util.control.TailCalls._

class AsmInterpreter {
  private[this] var input: FileInputStream = _
  private[this] var output: FileOutputStream = _
  private[this] var roughStep: Int = _

  private[this] var memory: Array[Word] = _
  private[this] val regs = mutable.Map[XReg, Word]()
  private[this] var functions: Map[String, FDef] = _
  private[this] var callStack: List[String] = _

  private[this] type Vars = Map[XVar, Word]

  private[this] def getWith(vars: Vars)(id: XID) = id match {
    case v: XVar => vars.getOrElse(v, ????(id))
    case r: XReg => regs(r)
  }

  private[this] def setWith(vars: Vars)(id: XID, w: Word) = id match {
    case v: XVar => vars + (v -> w)
    case r: XReg => regs(r) = w; vars
  }

  private[this] def getMemoryOrElse(index: Int, orElse: => Word) = {
    if (0 <= index && index < memory.length) memory(index)
    else orElse
  }

  private[this] def incRoughStep(msg: => String) = {
    roughStep += 1
    if ((roughStep & (1 << 19) - 1) == 0) {
      println(msg)
      output.flush()
    }
  }

  private[this] def interpret(scope: String, varsRec: TailRec[Vars], line: Line): TailRec[Vars] = {
    varsRec.flatMap { vars =>
      incRoughStep(s"$roughStep: ${vars.size}; $callStack")
      //      if (roughStep > 71936) ???
      //
      //      if (roughStep >= 71920) {
      //        println(roughStep)
      //        println(s"    $line")
      //        println(s"    $vars")
      //        println()
      //      }

      val get = getWith(vars) _
      val set = setWith(vars) _

      def getVC(vc: VC) = vc.fold(get, identity)

      val dest = line.dest
      val isDummy = line.dest == XReg.DUMMY
      line.inst match {
        case Mv(id) if !isDummy =>
          done(set(dest, get(id)))
        case Mvi(value) if !isDummy =>
          done(set(dest, value))
        case NewArray(len, elem) if !isDummy =>
          val h = regs(XReg.HEAP)
          val l = getVC(len).int
          for (i <- 0 until l) memory(h.int + i) = get(elem)
          regs(XReg.HEAP) = Word.fromInt(h.int + l)
          done(set(dest, h))
        case Store(addr, index, value) if isDummy =>
          val a = get(addr).int + getVC(index).int
          //          if (a == 0 || a == 44861 || a == 44862) {
          //            println(s"[$roughStep] store mem[$a] from ${memory(a)} to ${get(value)}: $line\n")
          //          }
          memory(a) = get(value)
          done(vars)
        case Load(addr, index) if !isDummy =>
          //          addr match {
          //            case XVar(ID("dirvecsA$gciG" | "dgroupA" | "$CrkA")) =>
          //              println(
          //                s"""[$roughStep] load mem[${get(addr)} + ${getVC(index)} = ${
          //                  get(addr).int + getVC(index).int
          //                }] = ${memory(get(addr).int + getVC(index).int)}
          //                   |    ${memory(0)};; $line
          //                   |""".stripMargin
          //              )
          //            case _ =>
          //          }

          val v = getMemoryOrElse(get(addr).int + getVC(index).int,
            ????(line, get(addr), getVC(index)))
          done(set(dest, v))
        case UnOpTree(op, value) if !isDummy =>
          val r = op match {
            case Floor => Word.fromFloat(get(value).float.floor)
            case Itof => Word.fromFloat(get(value).int.toFloat)
          }
          done(set(dest, r))
        case BinOpVCTree(op, left, right) if !isDummy =>
          done(set(dest, op.fn(get(left), getVC(right))))
        case BinOpVTree(op, left, right) if !isDummy =>
          try {
            // if (dest.idStr == "$CraB") println((line, get(left), get(right)))
            done(set(dest, op.fn(get(left), get(right))))
          } catch {
            case e: NotImplementedError =>
              println((line, get(left), get(right)))
              throw e
          }
        case Nop if isDummy => done(vars)
        case Read if !isDummy =>
          done(set(dest, Word.fromInt(input.read())))
        case Write(value) if isDummy =>
          output.write(get(value).int % 256)
          done(vars)
        case CallDir(fn, args, None) =>
          //          println(s"${memory(0)}; $fn(${args.map(get).mkString(", ")})")
          // TODO: save registers
          callStack ::= fn
          for (res <- tailcall(interpretFn(fn, args map get))) yield {
            //if (fn == "sqrtA") println((line, args map get, res))
            callStack = callStack.tail
            res match {
              case Some(res) if !isDummy => set(dest, res)
              case None if isDummy => vars
              case _ => ????(line, res)
            }
          }
        case _ => ????(line)
      }
    }
  }

  private[this] def interpretBlock(
    fun: FDef, bi0: BlockIndex, varsIn: Vars
  ): TailRec[Option[Word]] = {
    val Block(_, lines0, _, ji1) = fun.body.blocks(bi0)

    val varsOutRec = lines0.foldLeft(done(varsIn))((v, l) => tailcall(interpret(fun.name, v, l)))

    varsOutRec.flatMap { varsOut =>
      val get = getWith(varsOut) _
      incRoughStep(s"$roughStep: jump $ji1; $callStack")

      fun.body.jumps(ji1) match {
        case j1: StartFun => ????(j1)
        case Return(_, _, XReg.DUMMY, _) => done(None)
        case Return(_, _, value, _) => done(Some(get(value)))
        case Branch(_, _, Branch.Cond(op, left, right), _, tru2, fls2) =>
          val c = op.fn(get(left), right.fold(get, identity))
          tailcall(interpretBlock(fun, if (c) tru2 else fls2, varsOut))
        case Merge(_, _, _, XReg.DUMMY, bi2) =>
          tailcall(interpretBlock(fun, bi2, varsOut))
        case Merge(_, _, inputs, outputID, bi2) =>
          val vs = outputID match {
            case XReg.DUMMY => None
            case r: XReg =>
              regs(r) = get(inputs.find(_._2 == bi0).get._1)
              None
            case v: XVar => Some(v -> get(inputs.find(_._2 == bi0).get._1))
          }
          tailcall(interpretBlock(fun, bi2, varsOut ++ vs))
      }
    }
  }

  private[this] def interpretFn(name: String, args: List[Word]): TailRec[Option[Word]] = {
    if (name == ID.Special.ASM_EXIT_FUN) return done(None)

    incRoughStep(s"$roughStep: fun $name; $callStack")

    val fun = functions.getOrElse(name, ????(name, args))
    val vars: Vars = fun.args.zipStrict(args).flatMap {
      case (v: XVar, actual) => Some(v, actual)
      case (r: XReg, actual) =>
        if (r != XReg.DUMMY) regs(r) = actual
        None
    }.toMap
    tailcall(interpretBlock(fun, fun.body.blocks.firstKey, vars))
  }

  def apply(prog: Program, input: FileInputStream, output: FileOutputStream): String = {
    this.input = input
    this.output = output
    functions = prog.functions.map(f => f.name -> f).toMap
    roughStep = 0
    memory = Array.fill(1 << AnsCaml.config.memorySizeLog2)(Word.fromInt(0))
    callStack = Nil
    regs.clear()
    regs ++= XReg.toConstants
    regs(XReg.HEAP) = Word.fromInt(0)

    interpretFn(ID.Special.MAIN, Nil).result

    println(regs)
    println(s"[Asm Interpreter] Rough Step = $roughStep")

    output.toString
  }
}
