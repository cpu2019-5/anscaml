package net.akouryy.anscaml
package knorm
package debug

import java.io.{FileInputStream, FileOutputStream}

import base._
import syntax.{BinOp, CmpOp}

import scala.util.control.TailCalls._

class KNInterpreter {

  import KNInterpreter._

  private[this] var input: FileInputStream = _
  private[this] var output: FileOutputStream = _

  private[this] var roughStep: Long = _

  private[this] val stdlib =
    Map[String, List[Value] => Value](
      "print_char" -> { case List(VInt(i)) => output.write(i % 256); VTuple(Nil) },
      "read_char" -> { case List() => VInt(input.read()) },
      "fneg" -> { case List(VFloat(f)) => VFloat(-f) },
      "fabs" -> { case List(VFloat(f)) => VFloat(f.abs) },
      "fsqr" -> { case List(VFloat(f)) => VFloat(f * f) },
      "sqrt" -> { case List(VFloat(f)) => VFloat(Math.sqrt(f.toDouble).toFloat) },
      "floor" -> { case List(VFloat(f)) => VFloat(f.floor) },
      "float_of_int" -> { case List(VInt(i)) => VFloat(i.toFloat) },
      "int_of_float" -> { case List(VFloat(f)) => VInt((f + 0.5).toInt) },
      "bits_of_float" -> {
        case List(VFloat(f)) => VInt(java.lang.Float.floatToRawIntBits(f))
      },
      "float_of_bits" -> { case List(VInt(i)) => VFloat(java.lang.Float.intBitsToFloat(i)) },
    )

  private[this] def interpret(kc: KNorm, env: Map[ID, Value]): TailRec[Value] = {
    def get(id: ID) = env.getOrElse(id, !!!!(kc, id))

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
      case KNorm.LoopUpdater(elems) => done(VTuple(elems map get))
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
      case KNorm.Apply(fn, args, _) =>
        get(fn) match {
          case VFun(fn) => fn(args map get)
          case fn => !!!!(kc, fn)
        }
      case KNorm.ApplyExternal(fn, args) =>
        done(stdlib.getOrElse(fn.str, !!!!(kc, fn))(args map get))
      case KNorm.IfCmp(op, left, right, tru, fls) =>
        val cond = (op, get(left), get(right)) match {
          case (CmpOp.II(fn), VInt(l), VInt(r)) => fn(l, r)
          case (CmpOp.FF(fn), VFloat(l), VFloat(r)) => fn(l, r)
          case (_, l, r) => !!!!(kc, l, r)
        }
        if (cond) interpret(tru, env)
        else interpret(fls, env)
      case KNorm.ForCmp(op, left, right, negated, loopVars, initVars, body, kont) =>
        val newEnv = env ++ loopVars.zipStrict(initVars map get)
        val cond = (op, newEnv(left), newEnv(right)) match {
          case (CmpOp.II(fn), VInt(l), VInt(r)) => fn(l, r)
          case (CmpOp.FF(fn), VFloat(l), VFloat(r)) => fn(l, r)
          case (_, l, r) => !!!!(kc, l, r)
        }
        if (cond ^ negated) {
          for {
            b <- tailcall(interpret(body, newEnv))
            l <- tailcall(interpret(kc, env ++ loopVars.zipStrict(b.asInstanceOf[VTuple].v)))
          } yield {
            l
          }
        } else {
          tailcall(interpret(kont, newEnv))
        }
      case KNorm.Let(Entry(name, _), bound, kont) =>
        for {
          b <- tailcall(interpret(bound, env))
          k <- tailcall(interpret(kont, env + (name -> b)))
        } yield k
      case KNorm.LetTuple(elems, bound, kont) =>
        get(bound) match {
          case VTuple(bs) if elems.size == bs.size =>
            interpret(kont, env ++ elems.zipMap(bs)((e, b) => e.name -> b))
          case b => !!!!(kc, b)
        }
      case KNorm.LetRec(KNorm.FDef(Entry(name, _), args, body, _), kont) =>
        lazy val f: VFun = VFun((act: List[Value]) => {
          interpret(body, env ++ args.map(_.name).zipStrict(act) + (name -> f))
        })
        interpret(kont, env + (name -> f))
    }
  }

  def apply(prog: KNorm, input: FileInputStream, output: FileOutputStream): String = {
    this.input = input
    this.output = output
    roughStep = 0

    interpret(prog, Map()).result

    println(s"[KN Interpreter] Rough Step = $roughStep")

    output.toString
  }
}

object KNInterpreter {

  sealed trait Value

  final case class VInt(i: Int) extends Value

  final case class VFloat(f: Float) extends Value

  final case class VArray(v: Array[Value]) extends Value {
    override def toString = s"VArray(${v.mkString(", ")})"
  }

  final case class VTuple(v: List[Value]) extends Value

  final case class VFun(fn: List[Value] => TailRec[Value]) extends Value {
    override val toString = "VFun(...)"
  }

}
