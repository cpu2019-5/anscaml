package net.akouryy.anscaml
package syntax

import scala.util.parsing.input.Positional

import base.ID

sealed trait LexToken

object LexToken {

  class Positioned(val token: LexToken) extends Positional {
    override def toString: String = s"[$pos]$token"
  }

  final case class BOOL(b: Boolean) extends LexToken

  final case class INT(i: Int) extends LexToken

  final case class FLOAT(b: Float) extends LexToken

  final case class IDENT(id: ID) extends LexToken

  case object PLUS extends LexToken

  case object MINUS extends LexToken

  case object ASTER extends LexToken

  case object SLASH extends LexToken

  case object MOD extends LexToken

  case object PLUS_DOT extends LexToken

  case object MINUS_DOT extends LexToken

  case object ASTER_DOT extends LexToken

  case object SLASH_DOT extends LexToken

  case object EQUAL extends LexToken

  case object NOT_EQUAL extends LexToken

  case object LESS extends LexToken

  case object LESS_EQUAL extends LexToken

  case object GREATER extends LexToken

  case object GREATER_EQUAL extends LexToken

  case object EQUAL_DOT extends LexToken

  case object NOT_EQUAL_DOT extends LexToken

  case object LESS_DOT extends LexToken

  case object LESS_EQUAL_DOT extends LexToken

  case object GREATER_DOT extends LexToken

  case object GREATER_EQUAL_DOT extends LexToken

  case object IF extends LexToken

  case object THEN extends LexToken

  case object ELSE extends LexToken

  case object LET extends LexToken

  case object IN extends LexToken

  case object REC extends LexToken

  case object NO_INLINE extends LexToken

  case object COMMA extends LexToken

  case object DOT extends LexToken

  case object ASSIGN extends LexToken

  case object SEMICOLON extends LexToken

  case object L_PAREN extends LexToken

  case object R_PAREN extends LexToken

  case object EOF extends LexToken

}
