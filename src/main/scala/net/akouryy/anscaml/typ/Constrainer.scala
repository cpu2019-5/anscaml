package net.akouryy.anscaml
package typ

import base._
import syntax._
import Typ.Lit

class Constrainer {

  import Constrainer._

  private[this] var constraints: List[Constraint] = Nil

  private[this] def constrainRec(tree: Syntax, typEnv: Map[ID, Typ]): Typ = {
    import Syntax._

    tree match {
      case LitBool(b) => Typ.Bool(Lit.List[Primitives.Bool](Set(b)))
      case LitInt(i) => Typ.Int(Lit.List[Primitives.Int](Set(i)))
      case LitFloat(f) => Typ.Float(Lit.List[Primitives.Float](Set(f)))
      case Not(s) =>
        val t = constrainRec(s, typEnv)
        constraints ::= Typ.Bool(Lit.All()) >:> t
        Typ.BoolAll
      case BinOpTree(op, sl, sr) =>
        val tl = constrainRec(sl, typEnv)
        val tr = constrainRec(sr, typEnv)
        constraints :::= List(op.prim.typ >:> tl, op.prim.typ >:> tr)
        op.prim.typ
      case CmpOpTree(op, sl, sr) =>
        val tl = constrainRec(sl, typEnv)
        val tr = constrainRec(sr, typEnv)
        constraints :::= List(op.prim.typ >:> tl, op.prim.typ >:> tr)
        Typ.BoolAll
      case If(sc, st, sf) =>
        val tc = constrainRec(sc, typEnv)
        val tt = constrainRec(st, typEnv)
        val tf = constrainRec(sf, typEnv)
        val commonTyp = Typ.generateTypVar()
        constraints :::= List(Typ.BoolAll >:> tc, commonTyp >:> tt, commonTyp >:> tf)
        commonTyp
      case Let(Entry(v, tv), sb, sk) =>
        val tb = constrainRec(sb, typEnv)
        val tk = constrainRec(sk, typEnv + (v -> tv))
        constraints ::= tv >:> tb
        tk
      case Var(v) =>
        typEnv.get(v) orElse ExtEnv.get(v) getOrElse ???
      case LetRec(FDef(Entry(f, tf), args, sb, _), sk) =>
        val tb = constrainRec(sb, typEnv + (f -> tf) ++ args.map(a => a.name -> a.typ))
        val tk = constrainRec(sk, typEnv + (f -> tf))
        constraints ::= tf >:> Typ.Fun(args.map(_.typ), tb)
        tk
      case Apply(sf, sas) =>
        val tf = constrainRec(sf, typEnv)
        val tas = sas.map(constrainRec(_, typEnv))
        val resultTyp = Typ.generateTypVar()
        constraints ::= Typ.Fun(tas, resultTyp) >:> tf
        resultTyp
      case Tuple(ses) =>
        Typ.Tuple(ses.map(constrainRec(_, typEnv)))
      case LetTuple(elems, sb, sk) =>
        val tb = constrainRec(sb, typEnv)
        val tk = constrainRec(sk, typEnv ++ elems.map(e => e.name -> e.typ))
        constraints ::= Typ.Tuple(elems.map(_.typ)) >:> tb
        tk
      case Array(sl, se) =>
        val tl = constrainRec(sl, typEnv)
        val te = constrainRec(se, typEnv)
        val elemTyp = Typ.generateTypVar()
        constraints :::= List(elemTyp >:> te, Typ.IntAll >:> tl)
        Typ.Array(elemTyp)
      case Get(sa, si) =>
        val ta = constrainRec(sa, typEnv)
        val ti = constrainRec(si, typEnv)
        val elemTyp = Typ.generateTypVar()
        constraints :::= List(Typ.Array(elemTyp) >:> ta, Typ.IntAll >:> ti)
        elemTyp
      case Put(sa, si, sv) =>
        val ta = constrainRec(sa, typEnv)
        val ti = constrainRec(si, typEnv)
        val tv = constrainRec(sv, typEnv)
        val elemTyp = Typ.generateTypVar()
        constraints :::= List(ta >:> Typ.Array(elemTyp), Typ.IntAll >:> ti, elemTyp >:> tv)
        Typ.Unit
    }
  }

  def constrain(tree: Syntax): Seq[Constraint] = {
    constraints = Nil
    constrainRec(tree, Map())
    constraints
  }
}

object Constrainer {

  sealed trait Constraint

  final case class >:>(t1: Typ, t2: Typ) extends Constraint

  final case class =:=(t1: Typ, t2: Typ) extends Constraint

  implicit class TypOps(val t1: Typ) extends AnyVal {
    def >:>(t2: Typ): >:> = Constrainer.>:>(t1, t2)

    def =:=(t2: Typ): =:= = Constrainer.=:=(t1, t2)
  }

  val ExtEnv: Map[ID, Typ.Fun] = Map(
    ID("print_char") -> Typ.Fun(List(Typ.IntAll), Typ.Unit),
    ID("read_char") -> Typ.Fun(List(Typ.Unit), Typ.IntAll),
    ID("fneg") -> Typ.Fun(List(Typ.FloatAll), Typ.FloatAll),
    ID("fabs") -> Typ.Fun(List(Typ.FloatAll), Typ.FloatAll),
    ID("fsqr") -> Typ.Fun(List(Typ.FloatAll), Typ.FloatAll),
    ID("fhalf") -> Typ.Fun(List(Typ.FloatAll), Typ.FloatAll),
    ID("floor") -> Typ.Fun(List(Typ.FloatAll), Typ.FloatAll),
    ID("float_of_int") -> Typ.Fun(List(Typ.IntAll), Typ.FloatAll),
    ID("bits_of_float") -> Typ.Fun(List(Typ.FloatAll), Typ.IntAll),
    ID("float_of_bits") -> Typ.Fun(List(Typ.IntAll), Typ.FloatAll),
  )
}
