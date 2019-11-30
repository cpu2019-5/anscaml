package net.akouryy.anscaml
package typ

import base._
import Constrainer.{=:=, >:>, TypOps, Constraint => TypConstraint}

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * # 単一化
  *
  * この言語は単なる Int や Array Float 等の型だけでなく、
  * Int{0 | 512} や Array Float{0.5} などといったリテラル型を持つ。
  * 以下、単なる型を Int{any} や Array Float{any} 等と表す。
  * これらの型の間には包含関係 Int{any} ⊇ Int{0 | 512} ⊇ Int{0} や
  * Fun (Bool{true} -> Float{any}) ⊇ Fun (Bool{any} -> Float{0.5}) が成立する。
  * 単一化関数にも、型の等値ではなく型の包含関係が制約として渡される。
  *
  * これを単一化するため、制約解消は2段階に分けて行われる。
  * 第一段階では、包含関係が全て等値関係であると考えて最汎単一化子を求める。
  * この結果のうち Int{...} や Array Float{...} の部分は最終的に求めたい結果と一致している。
  * 従ってこの部分(exteriorと呼ぶ)を採用する。
  *
  * 第二段階では、exteriorが等しい型変数の中で包含関係を単一化する。
  * この際最汎単一化子は一意に定まらない(極汎?)。言語仕様として、{α: Int{any}} という極汎単一化子と
  * {α: Int{0 | 512}} という極汎単一化子では後者を(つまりより厳しい型を)優先することを定める。
  * 包含関係を有向辺と見做したグラフを強連結成分分解してトポロジカル順序の逆順に可能な最も厳しい型を
  * 与えていけばよい。各強連結成分には同じ型が与えられる。
  *
  * ## 先行研究
  *
  * 不明
  */
object Unifier {

  type VarConstraint = (Typ.TypVar, Typ.TypVar)

  sealed trait LitConstraint[T]

  final case class >%>[T <: Primitives.IFB](l1: Lit[T], l2: Lit[T]) extends LitConstraint[T]

  implicit class LitOps[T <: Primitives.IFB](val l1: Lit[T]) extends AnyVal {
    def >%>(l2: Lit[T]): >%>[T] = Unifier.>%>(l1, l2)
  }

  final case class ExteriorResult(
    tsb: TypSubst,
    boolCrs: List[LitConstraint[Primitives.PBool]],
    intCrs: List[LitConstraint[Primitives.PInt]],
    floatCrs: List[LitConstraint[Primitives.PFloat]],
  )

  def unify(tcr: List[TypConstraint]): TypSubst = {
    val er = new Exterior().exterior(tcr)
    val boolSubst = new Interior().interior(er.boolCrs, 1, Lit.Var.count)
    val intSubst = new Interior().interior(er.intCrs, 2, Lit.Var.count)
    val floatSubst = new Interior().interior(er.floatCrs, 2, Lit.Var.count)

    floatSubst * (intSubst * (boolSubst * er.tsb))
  }

  class Exterior {

    private[this] var typCrs =
      List[ORef[Option[TypConstraint]]]()
    private[this] val typCrRefs =
      mutable.Map[Typ.TypVar, List[ORef[Option[TypConstraint]]]]().withDefault(_ => Nil)
    private[this] var varCrs =
      List[ORef[Option[VarConstraint]]]()
    private[this] val varCrRefs =
      mutable.Map[Typ.TypVar, List[ORef[Option[VarConstraint]]]]().withDefault(_ => Nil)

    private[this] val typSubst =
      new TypSubst(mutable.Map())
    private[this] var boolCrs =
      List[LitConstraint[Primitives.PBool]]()
    private[this] var intCrs =
      List[LitConstraint[Primitives.PInt]]()
    private[this] var floatCrs =
      List[LitConstraint[Primitives.PFloat]]()

    private[this] def extendTypCrs(toAdd: List[TypConstraint]) = {
      for (tcr <- toAdd) {
        val ref = new ORef[Option[TypConstraint]](Some(tcr))
        typCrs ::= ref
        for (fv <- tcr.freeVariables) {
          typCrRefs(fv) ::= ref
        }
      }
    }

    private[this] def extendVarCrs(toAdd: List[VarConstraint]) = {
      for (vcr @ (v, w) <- toAdd) {
        val ref = new ORef[Option[VarConstraint]](Some(vcr))
        varCrs ::= ref
        varCrRefs(v) ::= ref
        varCrRefs(w) ::= ref
      }
    }

    private[this] def exteriorVar(v: Typ.TypVar, typ: Typ) = {
      if (typ.freeVariables contains v) {
        ???
      }

      val tsb = new TypSubst(mutable.Map(v -> typ))

      for (ref @ ORef(Some((x, y))) <- varCrRefs(v)) {
        ref := None
        extendTypCrs(List(tsb * (x >:> y)))
      }
      varCrRefs.remove(v)

      for (ref @ ORef(Some(tcr)) <- typCrRefs(v)) {
        ref := None
        extendTypCrs(List(tsb * tcr))
      }
      typCrRefs.remove(v)

      typSubst <<= tsb
    }

    def exterior(typConstraints0: List[TypConstraint]): ExteriorResult = {
      extendTypCrs(typConstraints0)

      while (true) {
        typCrs match {
          case Nil => varCrs match {
            case Nil =>
              return ExteriorResult(typSubst, boolCrs, intCrs, floatCrs)
            case ORef(None) :: rest =>
              varCrs = rest
            case (ref @ ORef(Some((v, w)))) :: rest =>
              ref := None
              println(s"[TypUnify] Unknown TypVar $$${v.num} = $$${w.num}. Using TInt{}.")
              extendTypCrs(List(w >:> Typ.TInt(Lit.Var.generate())))
              varCrs = rest
          }
          case ORef(None) :: rest =>
            typCrs = rest
          case (ref @ ORef(Some(tcr))) :: rest =>
            val (sameRelation, t1, t2) = tcr match {
              case t1 =:= t2 => (=:=, t1, t2)
              case t1 >:> t2 => (>:>, t1, t2)
            }

            (t1, t2) match {
              case (a, b) if a == b =>
                ref := None
                typCrs = rest
              case (Typ.TBool(l1), Typ.TBool(l2)) =>
                ref := None
                typCrs = rest
                boolCrs ::= l1 >%> l2
              case (Typ.TInt(l1), Typ.TInt(l2)) =>
                ref := None
                typCrs = rest
                intCrs ::= l1 >%> l2
              case (Typ.TFloat(l1), Typ.TFloat(l2)) =>
                ref := None
                typCrs = rest
                floatCrs ::= l1 >%> l2
              case (Typ.Fun(as1, r1), Typ.Fun(as2, r2)) if as1.sizeCompare(as2) == 0 =>
                ref := None
                typCrs = rest
                extendTypCrs(sameRelation(r1, r2) :: as1.zip(as2).map {
                  case (a1, a2) => sameRelation(a2, a1) // 反変
                })
              case (Typ.Tuple(es1), Typ.Tuple(es2)) if es1.sizeCompare(es2) == 0 =>
                ref := None
                typCrs = rest
                extendTypCrs(es1.zip(es2).map { case (e1, e2) => sameRelation(e1, e2) })
              case (Typ.Array(e1), Typ.Array(e2)) =>
                ref := None
                typCrs = rest
                extendTypCrs(List(e1 =:= e2)) // 非変
              case (v: Typ.TypVar, w: Typ.TypVar) if v != w =>
                ref := None
                typCrs = rest
                extendVarCrs(List((v, w)))
              case (t, v: Typ.TypVar) =>
                exteriorVar(v, t.withNewVars())
              case (v: Typ.TypVar, t) =>
                exteriorVar(v, t.withNewVars())
              case _ => ???
            }
        }
      }

      ???
    }

  }

  class Interior[T <: Primitives.IFB : ClassTag] {
    def interior(litCrs: List[LitConstraint[T]], listLimit: Int, nLitVar: Int): LitSubst[T] = {
      val directLowerBounds = mutable.Map[Lit.Var[T], Lit[T]]().withDefaultValue(Lit.Empty)
      for {
        (v @ Lit.Var(_)) >%> (lit @ (Lit.List(_) | Lit.All())) <- litCrs
      } {
        directLowerBounds(v) = (lit | directLowerBounds(v)) (listLimit)
      }

      val graph = algo.Graph(nLitVar,
        for {
          Lit.Var(v) >%> Lit.Var(w) <- litCrs
        } yield (v, w)
      )

      val quo = graph.sccQuotient
      val lsb = new LitSubst[T](mutable.Map())

      for (i <- 0 until quo.graph.nv) {
        val lowerBound =
          quo.fromSCCIndex(i).foldLeft(Lit.Empty: Lit[T])(
            (l, v) => (l | directLowerBounds(Lit.Var(v))) (listLimit)
          )

        val lit =
          quo.graph.destss(i).foldLeft(lowerBound)(
            (l, j) => (l | lsb.mapping(Lit.Var(quo.fromSCCIndex(j).head))) (listLimit)
          )

        for {
          vi <- quo.fromSCCIndex(i)
          v = Lit.Var[T](vi)
        } {
          assert(!lsb.mapping.contains(v))
          lsb.mapping(v) = lit
        }
      }

      lsb
    }
  }

}
