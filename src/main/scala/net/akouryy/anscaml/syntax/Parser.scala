package net.akouryy.anscaml
package syntax

import base._
import LexToken._

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}

object AnsParser extends Parsers {
  override type Elem = LexToken.Positioned

  class LexTokenReader(tokens: List[Elem]) extends Reader[Elem] {
    override def first: Elem = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.fold[Position](NoPosition)(_.pos)

    override def rest: Reader[Elem] = new LexTokenReader(tokens.tail)
  }

  private[this] def binOpBuilder[T <: Primitives.IF](op: BinOp[T])(l: Syntax, r: Syntax) =
    Syntax.BinOpTree(op, l, r)

  private[this] val BinaryOperators =
    Array[Map[LexToken, (Syntax, Syntax) => Syntax]](
      Map(
        EQUAL -> ((l, r) => Syntax.CmpOpTree(CmpOp.Eq, l, r)),
        NOT_EQUAL -> ((l, r) => Syntax.Not(Syntax.CmpOpTree(CmpOp.Eq, l, r))),
        LESS -> ((l, r) => Syntax.CmpOpTree(CmpOp.Le, l, r)),
        GREATER -> ((l, r) => Syntax.CmpOpTree(CmpOp.Le, r, l)),
        LESS_EQUAL -> ((l, r) => Syntax.Not(Syntax.CmpOpTree(CmpOp.Le, r, l))),
        GREATER_EQUAL -> ((l, r) => Syntax.Not(Syntax.CmpOpTree(CmpOp.Le, l, r))),

        EQUAL_DOT -> ((l, r) => Syntax.CmpOpTree(CmpOp.Feq, l, r)),
        NOT_EQUAL_DOT -> ((l, r) => Syntax.Not(Syntax.CmpOpTree(CmpOp.Feq, l, r))),
        LESS_DOT -> ((l, r) => Syntax.CmpOpTree(CmpOp.Fle, l, r)),
        GREATER_DOT -> ((l, r) => Syntax.CmpOpTree(CmpOp.Fle, r, l)),
        LESS_EQUAL_DOT -> ((l, r) => Syntax.Not(Syntax.CmpOpTree(CmpOp.Fle, r, l))),
        GREATER_EQUAL_DOT -> ((l, r) => Syntax.Not(Syntax.CmpOpTree(CmpOp.Fle, l, r))),
      ),
      Map(
        PLUS -> binOpBuilder(BinOp.Add),
        MINUS -> binOpBuilder(BinOp.Sub),
        PLUS_DOT -> binOpBuilder(BinOp.Fadd),
        MINUS_DOT -> binOpBuilder(BinOp.Fsub),
      ),
      Map(
        ASTER -> binOpBuilder(BinOp.Mul),
        SLASH -> binOpBuilder(BinOp.Div),
        MOD -> binOpBuilder(BinOp.Mod),
        ASTER_DOT -> binOpBuilder(BinOp.Fmul),
        SLASH_DOT -> binOpBuilder(BinOp.Fdiv),
      ),
    )

  private[this] implicit def acceptPositioned(t: LexToken): Parser[Elem] =
    acceptIf(_.token == t)(_ => s"expected $t")

  private[this] def acceptPositioned[T](expected: String, f: PartialFunction[LexToken, T]) =
    accept(expected, { e: Elem => f.unapply(e.token) }.unlift)

  private[this] def identifier =
    acceptPositioned("IDENT", { case IDENT(id) => id })

  private[this] def dottableExpr: Parser[Syntax] =
    L_PAREN ~> expr <~ R_PAREN |
    L_PAREN ~ R_PAREN ^^ { _ => Syntax.LitUnit } |
    L_PAREN ~> repsep(expr, COMMA) <~ R_PAREN ^^ Syntax.Tuple |
    identifier ^^ Syntax.Var |
    acceptPositioned("BOOL", { case BOOL(b) => Syntax.LitBool(b) }) |
    acceptPositioned("INT", { case INT(i) => Syntax.LitInt(i) }) |
    acceptPositioned("FLOAT", { case FLOAT(f) => Syntax.LitFloat(f) })

  private[this] def simpleExpr =
    dottableExpr ~ rep(DOT ~ L_PAREN ~> expr <~ R_PAREN) ^^ {
      case a ~ is => is.foldLeft(a)(Syntax.Get)
    }

  private[syntax] def expr: Parser[Syntax] =
    semiableExpr ~ opt(SEMICOLON ~! opt(expr)) ^^ {
      case l ~ (None | Some(_ ~ None)) => l
      case l ~ Some(_ ~ Some(r)) => Syntax.LetTuple(Nil, l, r)
    }

  /**
    * セミコロンの両側に来れる式
    * 注意: if a then b else c; d は (if a then b else c); d となる一方、
    * if a then b else let x = y in c; d は if a then b else let x = y in (c; d) となる
    */
  private[this] def semiableExpr: Parser[Syntax] =
    IF ~> expr ~ (THEN ~> expr) ~ (ELSE ~> semiableExpr) ^^ { case i ~ t ~ f =>
      Syntax.If(i, t, f)
    } |
    LET ~> identifier ~ (EQUAL ~> expr) ~ (IN ~> expr) ^^ { case id ~ bound ~ kont =>
      Syntax.Let(Entry.generate(id), bound, kont)
    } |
    LET ~ REC ~> identifier ~ opt(NO_INLINE) ~ rep1(identifier) ~ (EQUAL ~> expr) ~ (IN ~> expr) ^^ {
      case id ~ noInline ~ args ~ bound ~ kont =>
        Syntax.LetRec(
          Syntax.FDef(Entry.generate(id), args.map(Entry.generate), bound, noInline.nonEmpty),
          kont,
        )
    } |
    LET ~ L_PAREN ~> rep1sep(identifier, COMMA) ~ (R_PAREN ~> EQUAL ~> expr) ~ (IN ~> expr) ^^ {
      case elems ~ bound ~ kont => Syntax.LetTuple(elems.map(Entry.generate), bound, kont)
    } |
    putExpr

  private[this] def putExpr: Parser[Syntax] =
    simpleExpr ~ (DOT ~> L_PAREN ~> expr <~ R_PAREN <~ ASSIGN) ~ putExpr ^^ { case a ~ i ~ v =>
      Syntax.Put(a, i, v)
    } |
    binaryExpr(0)

  private[this] def binaryExpr(pred: Int): Parser[Syntax] =
    if (pred >= 3) applyExpr
    else
      binaryExpr(pred + 1) ~ rep(
        BinaryOperators(pred).map { case (k, v) => k ^^^ v }.reduceLeft(_ | _) ~
        binaryExpr(pred + 1)
      ) ^^ {
        case term0 ~ opTerms =>
          opTerms.foldLeft(term0) { case (acc, op ~ term) => op(acc, term) }
      }

  private[this] def applyExpr =
    simpleExpr ~ rep(simpleExpr) ^^ {
      case expr ~ Nil => expr
      case fn ~ args => Syntax.Apply(fn, args)
    }
}

object Parser {

  final case class ParseException(detail: String, next: AnsParser.Input)

  def parse(tokens: List[LexToken.Positioned]): Syntax = {
    AnsParser.expr(new AnsParser.LexTokenReader(tokens)) match {
      case AnsParser.Success(result, _) => result
      case AnsParser.NoSuccess(message, next) =>
        throw new RuntimeException(ParseException(message, next).toString)
    }
  }
}
