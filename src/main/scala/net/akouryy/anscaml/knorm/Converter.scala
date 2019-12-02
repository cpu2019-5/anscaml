package net.akouryy.anscaml
package knorm

import base._
import syntax.{BinOp, CmpOp, Syntax}
import typ.{Lit, Typ}

object Converter {
  type TypedK = (Typ, KNorm.KRaw)

  final case class Mapper[A, B, C](map: (A => B) => C) {
    val flatMap: map.type = map
    val andThen: map.type = map

    def withFilter(@annotation.unused _x: A => Boolean): this.type = this
  }

  private[this] def insert(tk: TypedK, env: Map[ID, Typ]) =
    Mapper[(ID, Map[ID, Typ]), TypedK, TypedK](kont =>
      tk match {
        case (_, KNorm.Var(x)) => kont(x, env)
        case (t, e) =>
          val x = ID.generate()
          val (kt, ke) = kont(x, env + (x -> t))
          (kt, KNorm.Let(Entry(x, t), KNorm(e), KNorm(ke)))
      }
    )

  private[this] def insertMulti(subjects: List[Syntax], env: Map[ID, Typ]) =
    Mapper[(List[ID], Map[ID, Typ]), TypedK, TypedK] { kont =>
      val extraInfo = subjects.map(convert(env, _) match {
        case (_, KNorm.Var(x)) => Left(x)
        case (t, e) => Right(ID.generate(), t, e)
      })
      val (kt, ke) = kont(
        extraInfo.map(_.fold(identity, _._1)),
        env ++ extraInfo.flatMap { case Right((x, t, _)) => Some(x -> t); case Left(_) => None },
      )
      (
        kt,
        extraInfo.foldLeft(ke) {
          case (ke, Left(_)) => ke
          case (ke, Right((x, t, e))) =>
            KNorm.Let(Entry(x, t), KNorm(e), KNorm(ke))
        }
      )
    }

  private[this] def convertTyp(typ: Typ): Typ = typ match {
    case Typ.BoolAll => Typ.IntList(0, 1)
    case Typ.TBool(Lit.List(ls)) => Typ.IntList(ls.map(if (_) 1 else 0).toSeq: _*)
    case Typ.TBool(Lit.Var(_)) => ???
    case _ => typ.recursively(convertTyp)
  }

  private[this] def convertEntry(entry: Entry) = Entry(entry.name, convertTyp(entry.typ))

  private[this] def convert(env: Map[ID, Typ], ast: Syntax): TypedK = {
    import Syntax._

    ast match {
      case LitBool(b) =>
        val i = if (b) 1 else 0
        (Typ.IntList(i), KNorm.KInt(i))
      case LitInt(i) => (Typ.IntList(i), KNorm.KInt(i))
      case LitFloat(f) => (Typ.FloatList(f), KNorm.KFloat(f))
      case Not(s) => convert(env, BinOpTree(BinOp.Sub, LitInt(1), s))
      case BinOpTree(op, left, right) =>
        for {
          (x, env2) <- insert(convert(env, left), env)
          (y, _) <- insert(convert(env2, right), env2)
        } yield {
          (op.retTyp, KNorm.BinOpTree(op, x, y))
        }
      case cmp: CmpOpTree => /* Ifで拾われなかった場合 */
        convert(env, If(cmp, LitBool(true), LitBool(false)))
      case If(Not(cond), tru, fls) => convert(env, If(cond, fls, tru))
      case If(CmpOpTree(op, left, right), tru, fls) =>
        for {
          (l, env2) <- insert(convert(env, left), env)
          (r, env3) <- insert(convert(env2, right), env2)
        } yield {
          val (tt, te) = convert(env3, tru)
          val (ft, fe) = convert(env3, fls)
          try {
            (tt | ft, KNorm.IfCmp(op, l, r, KNorm(te), KNorm(fe)))
          } catch {
            case e: NotImplementedError => pprinter.pprintln(te); pprinter.pprintln(fe); throw e
          }
        }
      case If(cond, tru, fls) =>
        convert(env, If(CmpOpTree(CmpOp.Eq, cond, LitBool(false)), fls, tru))
      case Let(entry, bound, kont) =>
        val newEntry = convertEntry(entry)
        val (_, be) = convert(env, bound)
        val (kt, ke) = convert(env + newEntry.toPair, kont)
        (kt, KNorm.Let(newEntry, KNorm(be), KNorm(ke)))
      case Var(x) =>
        (env(x), KNorm.Var(x)) // TODO?: external function
      case LetRec(FDef(entry, args, body, noInline), kont) =>
        val newEntry = convertEntry(entry)
        val newArgs = args.map(convertEntry)
        val envWithFn = env + newEntry.toPair
        val (_, be) = convert(envWithFn ++ newArgs.map(_.toPair), body)
        val (kt, ke) = convert(envWithFn, kont)
        (kt, KNorm.LetRec(KNorm.FDef(newEntry, newArgs, KNorm(be), noInline), KNorm(ke)))
      case Apply(Var(fn), args) if !(env contains fn) =>
        val Typ.TFun(_, retTyp) = typ.Constrainer.ExtEnv(fn)
        for {
          (xs, _) <- insertMulti(args, env)
        } yield {
          (retTyp, KNorm.ApplyExternal(fn, xs))
        }
      case Apply(fn, args) =>
        val fnr @ (Typ.TFun(_, retTyp), _) = convert(env, fn): @unchecked
        for {
          (x, env2) <- insert(fnr, env)
          (ys, _) <- insertMulti(args, env2)
        } yield {
          (retTyp, KNorm.Apply(x, ys))
        }
      case Tuple(es) =>
        for ((xs, env2) <- insertMulti(es, env)) yield (
          Typ.TTuple(xs.map(env2)),
          KNorm.KTuple(xs)
        )
      case LetTuple(elems, bound, kont) =>
        for ((x, env2) <- insert(convert(env, bound), env)) yield {
          if (elems.isEmpty) {
            convert(env2, kont) // xは副作用のために束縛されるが使用はされない
          } else {
            val newElems = elems.map(convertEntry)
            val (kt, ke) = convert(env2 ++ newElems.map(_.toPair), kont)
            (kt, KNorm.LetTuple(newElems, x, KNorm(ke)))
          }
        }
      case Array(length, elem) =>
        for {
          (x, env2) <- insert(convert(env, length), env)
        } yield {
          val er @ (et, _) = convert(env2, elem)
          for {(y, _) <- insert(er, env2)} yield {
            (Typ.TArray(et), KNorm.Array(x, y))
          }
        }
      case Get(array, index) =>
        val ar @ (Typ.TArray(elemTyp), _) = convert(env, array)
        for {
          (x, env2) <- insert(ar, env)
          (y, _) <- insert(convert(env2, index), env2)
        } yield {
          (elemTyp, KNorm.Get(x, y))
        }
      case Put(array, index, value) =>
        for {
          (x, env2) <- insert(convert(env, array), env)
          (y, env3) <- insert(convert(env2, index), env2)
          (z, _) <- insert(convert(env3, value), env3)
        } yield {
          (Typ.TUnit, KNorm.Put(x, y, z))
        }
    }
  }

  def apply(ast: Syntax): KNorm = KNorm(convert(Map(), ast)._2)
}
