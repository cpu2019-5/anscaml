package net.akouryy.anscaml
package syntax

import scala.util.parsing.combinator.RegexParsers
import base.ID
import LexToken._

/**
  * 参考: http://enear.github.io/2016/03/31/parser-combinators/
  */
object Lexer extends RegexParsers {

  final case class LexException(msg: String, next: String /*scala.util.parsing.input.Position*/)

  private[this] def comment: Parser[_] =
    "(*" ~ rep(
      """( [^(*]++ | \(+ (?!\*) | \*+ (?!\)) )++""".replace(" ", "").r |
      comment
    ) ~ "*)"

  private[this] def w(s: String) = regex((s + "\\b").r)

  private[this] def bool =
    w("true") ^^^ BOOL(true) | w("false") ^^^ BOOL(false)

  private[this] def int =
    """(0|[1-9]\d*)\b""".r ^^ (i => INT(i.toInt))

  private[this] def float =
    """[+-]?(0|[1-9]\d*)\.\d+([eE][+-]?\d+)?\b""".r ^^ (f => FLOAT(f.toFloat))

  private[this] def ident =
    """[a-z][a-z0-9_]*\b""".r ^^ (str => IDENT(ID(str))) |
    "_" ^^ (_ => IDENT(ID.generate()))

  private[this] def annot =
    Annot.Annotations.map(x => s"[@$x]" ^^^ ANNOT(x)).reduce(_ | _)

  private[this] def others =
    w("if") ^^^ IF |
    w("then") ^^^ THEN |
    w("else") ^^^ ELSE |
    w("let") ^^^ LET |
    w("in") ^^^ IN |
    w("rec") ^^^ REC |
    "(" ^^^ L_PAREN |
    ")" ^^^ R_PAREN |
    "," ^^^ COMMA |
    "." ^^^ DOT |
    "<-" ^^^ ASSIGN |
    ";" ^^^ SEMICOLON |
    "+." ^^^ PLUS_DOT |
    "-." ^^^ MINUS_DOT |
    "*." ^^^ ASTER_DOT |
    "/." ^^^ SLASH_DOT |
    "=." ^^^ EQUAL_DOT |
    "<>." ^^^ NOT_EQUAL_DOT |
    "<=." ^^^ LESS_EQUAL_DOT |
    ">=." ^^^ GREATER_EQUAL_DOT |
    "<." ^^^ LESS_DOT |
    ">." ^^^ GREATER_DOT |
    "+" ^^^ PLUS |
    "-" ^^^ MINUS |
    "*" ^^^ ASTER |
    "/" ^^^ SLASH |
    "%" ^^^ MOD |
    "=" ^^^ EQUAL |
    "<>" ^^^ NOT_EQUAL |
    "<=" ^^^ LESS_EQUAL |
    ">=" ^^^ GREATER_EQUAL |
    "<" ^^^ LESS |
    ">" ^^^ GREATER

  private[this] def tokens =
    phrase(rep1(
      comment ^^^ None |
      positioned((bool | float | int | others | ident | annot) ^^ (new LexToken.Positioned(_)))
      ^^ (Some(_))
    )) ^^ (_.flatten)

  def lex(code: String): List[LexToken.Positioned] =
    parseAll(tokens, code) match {
      case Success(result, _) => result :+ new Positioned(EOF)
      case NoSuccess(message, next) =>
        throw new RuntimeException(LexException(message, next.toString).toString)
    }
}
