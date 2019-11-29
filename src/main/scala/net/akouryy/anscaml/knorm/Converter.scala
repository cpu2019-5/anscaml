package net.akouryy.anscaml
package knorm

import base._
import syntax.{BinOp, CmpOp, Syntax}
import typ.Typ

object Converter {
  type TypedK = (Typ, KNorm.Raw)

  final case class Mapper[A, B, C](map: (A => B) => C) {
    val flatMap: map.type = map
    val andThen: map.type = map
  }

  private[this] def insert(tk: TypedK) =
    Mapper[ID, TypedK, TypedK](kont =>
      tk match {
        case (_, KNorm.Var(x)) => kont(x)
        case (t, e) =>
          val x = ID.generate()
          val (kt, ke) = kont(x)
          (kt, KNorm.Let(Entry(x, t), KNorm(e), KNorm(ke)))
      }
    )

  private[this] def convert(env: Map[ID, Typ], ast: Syntax): TypedK = {
    import Syntax._

    def insertMulti(subjects: List[Syntax], env: Map[ID, Typ] = env) =
      Mapper[List[ID], TypedK, TypedK] { kont =>
        val extraInfo = subjects.map(convert(env, _) match {
          case (_, KNorm.Var(x)) => Left(x)
          case (t, e) => Right(ID.generate(), t, e)
        })
        val (kt, ke) = kont(extraInfo.map(_.fold(identity, _._1)))
        (
          kt,
          extraInfo.foldLeft(ke) {
            case (ke, Left(_)) => ke
            case (ke, Right((x, t, e))) =>
              KNorm.Let(Entry(x, t), KNorm(e), KNorm(ke))
          }
        )
      }

    ast match {
      case LitBool(b) => (Typ.IntAll, KNorm.KInt(if (b) 1 else 0))
      case LitInt(i) => (Typ.IntAll, KNorm.KInt(i))
      case LitFloat(f) => (Typ.FloatAll, KNorm.KFloat(f))
      case Not(s) => convert(env, BinOpTree(BinOp.Sub, LitInt(1), s))
      case BinOpTree(op, left, right) =>
        for {
          x <- insert(convert(env, left))
          y <- insert(convert(env, right))
        } yield {
          (op.prim.typ, KNorm.BinOpTree(op, x, y))
        }
      case cmp: CmpOpTree[_] => /* Ifで拾われなかった場合 */
        convert(env, If(cmp, LitBool(true), LitBool(false)))
      case If(Not(cond), tru, fls) => convert(env, If(cond, fls, tru))
      case If(CmpOpTree(op, left, right), tru, fls) =>
        for {
          l <- insert(convert(env, left))
          r <- insert(convert(env, right))
        } yield {
          val (tt, te) = convert(env, tru)
          val (_, fe) = convert(env, fls)
          (tt, KNorm.IfCmpTree(op, l, r, KNorm(te), KNorm(fe))) // TODO: 型
        }
      case If(cond, tru, fls) =>
        convert(env, If(CmpOpTree(CmpOp.Eq, cond, LitBool(false)), fls, tru))
      case Let(entry @ Entry(x, t), bound, kont) =>
        val (_, be) = convert(env, bound)
        val (kt, ke) = convert(env + (x -> t), kont)
        (kt, KNorm.Let(entry, KNorm(be), KNorm(ke)))
      case Var(x) =>
        (env(x), KNorm.Var(x)) // TODO?: external function
      case LetRec(FDef(entry @ Entry(fn, typ), args, body, noInline), kont) =>
        val envWithFn = env + (fn -> typ)
        val (_, be) = convert(envWithFn ++ args.map { case Entry(n, t) => (n, t) }, body)
        val (kt, ke) = convert(envWithFn, kont)
        (kt, KNorm.LetRec(KNorm.FDef(entry, args, KNorm(be), noInline), KNorm(ke)))
      case Apply(Var(fn), args) if !(env contains fn) =>
        val Typ.Fun(_, retTyp) = typ.Constrainer.ExtEnv(fn)
        for {
          xs <- insertMulti(args)
        } yield {
          (retTyp, KNorm.ExtFunApp(fn, xs))
        }
      case Apply(fn, args) =>
        val fnr @ (Typ.Fun(_, retTyp), _) = convert(env, fn): @unchecked
        for {
          x <- insert(fnr)
          ys <- insertMulti(args)
        } yield {
          (retTyp, KNorm.App(x, ys))
        }
      case Tuple(es) =>
        for (xs <- insertMulti(es)) yield (
          Typ.Tuple(xs.map(env.getOrElse(_, Typ.IntAll))), // TODO
          KNorm.Tuple(xs)
        )
      case LetTuple(elems, bound, kont) =>
        for (x <- insert(convert(env, bound))) yield {
          val (kt, ke) = convert(env ++ elems.map { case Entry(n, t) => (n, t) }, kont)
          (kt, KNorm.LetTuple(elems, x, KNorm(ke)))
        }
      case Array(length, elem) =>
        for {
          x <- insert(convert(env, length))
        } yield {
          val er @ (et, _) = convert(env, elem)
          for {y <- insert(er)} yield {
            (Typ.Array(et), KNorm.ExtFunApp(ID("create_array"), List(x, y)))
          }
        }
      case Get(array, index) =>
        val ar @ (Typ.Array(elemTyp), _) = convert(env, array)
        for {
          x <- insert(ar)
          y <- insert(convert(env, index))
        } yield {
          (elemTyp, KNorm.Get(x, y))
        }
      case Put(array, index, value) =>
        for {
          x <- insert(convert(env, array))
          y <- insert(convert(env, index))
          z <- insert(convert(env, value))
        } yield {
          (Typ.TUnit, KNorm.Put(x, y, z))
        }
    }
  }

  def apply(ast: Syntax): KNorm = KNorm(convert(Map(), ast)._2)
}
