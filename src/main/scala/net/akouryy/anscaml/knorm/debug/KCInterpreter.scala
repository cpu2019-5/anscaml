package net.akouryy.anscaml
package knorm
package debug

import java.io.{FileInputStream, FileOutputStream}

import base._
import KNorm.{CFDef, KCProgram, KClosed}
import net.akouryy.anscaml.syntax.{BinOp, CmpOp}

import scala.collection.mutable
import scala.util.control.TailCalls._

class KCInterpreter {

  import KCInterpreter._

  private[this] var input: FileInputStream = _
  private[this] var output: FileOutputStream = _

  private[this] val gConsts = mutable.Map[ID, Value]()

  private[this] var functions: Map[ID, List[Value] => TailRec[Value]] = _

  private[this] var roughStep: Int = _

  private[this] val stdlib =
    Map[String, PartialFunction[List[Value], Value]](
      "$ext_print_char" -> { case List(VInt(i)) => output.write(i % 256); VTuple(Nil) },
      "$ext_read_char" -> { case List() => VInt(input.read()) },
      "$ext_fneg" -> { case List(VFloat(f)) => VFloat(-f) },
      "$ext_fabs" -> { case List(VFloat(f)) => VFloat(f.abs) },
      "$ext_fsqr" -> { case List(VFloat(f)) => VFloat(f * f) },
      "$ext_floor" -> { case List(VFloat(f)) => VFloat(f.floor) },
      "$ext_float_of_int" -> { case List(VInt(i)) => VFloat(i.toFloat) },
      "$ext_bits_of_float" -> {
        case List(VFloat(f)) => VInt(java.lang.Float.floatToRawIntBits(f))
      },
      "$ext_float_of_bits" -> { case List(VInt(i)) => VFloat(java.lang.Float.intBitsToFloat(i)) },
    )

  private[this] def interpret(kc: KClosed, env: Map[ID, Value]): TailRec[Value] = {
    def get(id: ID) = env.getOrElse(id, gConsts.getOrElse(id, !!!!(kc, id)))

    def getFun(id: ID)(args: List[Value]) = {
      functions.get(id) match {
        case Some(f) => /*println(s"${id.str}(${args.mkString(", ")})");*/ f(args)
        case None => done(stdlib.getOrElse(id.str, !!!!(kc, id))(args))
      }
    }

    roughStep += 1
    if ((roughStep & (1 << 23) - 1) == 0) println(s"$roughStep ${env.size}")

    kc.raw match {
      case KNorm.KInt(i) => done(VInt(i))
      case KNorm.KFloat(f) => done(VFloat(f))
      case KNorm.BinOpTree(op, left, right) =>
        (op, get(left), get(right)) match {
          case (BinOp.III(op), VInt(l), VInt(r)) => done(VInt(op(l, r)))
          case (BinOp.FFF(op), VFloat(l), VFloat(r)) => done(VFloat(op(l, r)))
          case (_, l, r) => !!!!(kc, l, r)
        }
      case KNorm.Var(v) => done(get(v))
      case KNorm.KTuple(elems) => done(VTuple(elems map get))
      case KNorm.KArray(len, elem) =>
        get(len) match {
          case VInt(l) => done(VArray(Array.fill(l)(get(elem))))
          case l => !!!!(kc, l)
        }
      case KNorm.Get(array, index) =>
        (get(array), get(index)) match {
          case (VArray(a), VInt(i)) => done(a(i))
          case (a, i) => !!!!(kc, a, i)
        }
      case KNorm.Put(array, index, value) =>
        (get(array), get(index)) match {
          case (VArray(a), VInt(i)) => a(i) = get(value); done(VTuple(Nil))
          case (a, i) => !!!!(kc, a, i)
        }
      case KNorm.ApplyDirect(fn, args) =>
        tailcall(getFun(ID(fn))(args map get))
      case _: KNorm.ApplyClosure => ???
      case KNorm.CIfCmp(op, left, right, tru, fls) =>
        val cond = (op, get(left), get(right)) match {
          case (CmpOp.II(fn), VInt(l), VInt(r)) => fn(l, r)
          case (CmpOp.FF(fn), VFloat(l), VFloat(r)) => fn(l, r)
          case (_, l, r) => !!!!(kc, l, r)
        }
        if (cond) interpret(tru, env)
        else interpret(fls, env)
      case KNorm.CLet(Entry(name, _), bound, kont) =>
        for {
          b <- tailcall(interpret(bound, env))
          k <- tailcall(interpret(kont, env + (name -> b)))
        } yield k
      case KNorm.CLetTuple(elems, bound, kont) =>
        get(bound) match {
          case VTuple(bs) if elems.size == bs.size =>
            interpret(kont, env ++ elems.zipMap(bs)((e, b) => e.name -> b))
          case b => !!!!(kc, b)
        }
      case _: KNorm.CLetClosure => ???
      case _ => !!!!(kc)
    }
  }

  private[this] def interpretGC(entry: Entry, kc: KClosed) = {
    gConsts(entry.name) = interpret(kc, Map()).result
  }

  def apply(prog: KCProgram, input: FileInputStream, output: FileOutputStream): String = {
    this.input = input
    this.output = output
    gConsts.clear()
    roughStep = 0

    functions = prog.fDefs.map {
      case CFDef(Entry(name, _), args, _, body) =>
        name -> ((act: List[Value]) => {
          interpret(body, args.map(_.name).zipStrict(act).toMap)
        })
    }.toMap

    prog.gConsts.foreach(Function.tupled(interpretGC))

    interpret(prog.main, Map()).result

    println(s"[KC Interpreter] Rough Step = $roughStep")

    output.toString
  }
}

object KCInterpreter {

  sealed trait Value

  final case class VInt(i: Int) extends Value

  final case class VFloat(f: Float) extends Value

  final case class VArray(v: Array[Value]) extends Value {
    override def toString: String = s"VArray(${v.mkString(", ")})"
  }

  final case class VTuple(v: List[Value]) extends Value

}
