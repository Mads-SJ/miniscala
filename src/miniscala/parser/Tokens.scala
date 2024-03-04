package miniscala.parser

import scala.util.parsing.input.Positional

/**
  * Tokens used by the MiniScala lexer and parser.
  *
  * (You do *not* need to read this code!)
  */
object Tokens {

  sealed trait MiniScalaToken extends Positional

  case class OP(str: String) extends MiniScalaToken

  case class IDENTIFIER(str: String) extends MiniScalaToken

  case class SIMPLE_TYPE(str: String) extends MiniScalaToken

  case class STRING(str: String) extends MiniScalaToken

  case class INT(i: Int) extends MiniScalaToken

  case class BOOL(b: Boolean) extends MiniScalaToken

  case class FLOAT(v: Float) extends MiniScalaToken

  case class NULL() extends MiniScalaToken

  case class VVAL() extends MiniScalaToken

  case class VVAR() extends MiniScalaToken

  case class DDEF() extends MiniScalaToken

  case class DO() extends MiniScalaToken

  case class WWHILE() extends MiniScalaToken

  case class IFF() extends MiniScalaToken

  case class EELSE() extends MiniScalaToken

  case class LEFT_PAREN() extends MiniScalaToken

  case class RIGHT_PAREN() extends MiniScalaToken

  case class LEFT_BRACE() extends MiniScalaToken

  case class RIGHT_BRACE() extends MiniScalaToken

  case class EQ() extends MiniScalaToken

  case class COLON() extends MiniScalaToken

  case class COMMA() extends MiniScalaToken

  case class SEMICOLON() extends MiniScalaToken

  case class ARROW() extends MiniScalaToken

  case class MATCH() extends MiniScalaToken

  case class CASE() extends MiniScalaToken

  case class CLASS() extends MiniScalaToken

  case class DOT() extends MiniScalaToken

  case class NEW() extends MiniScalaToken
}
