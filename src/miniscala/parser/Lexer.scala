package miniscala.parser

import Parser.SyntaxError
import Tokens._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Position

/**
  * Lexer for MiniScala.
  *
  * (You do *not* need to read this code!)
  */
object Lexer extends RegexParsers {

  override def skipWhitespace = true

  override val whiteSpace: Regex = """(?:\s|//.*|/\*(?:.|[\n\r])*?\*/)+""".r

  private val keywords = Set("true", "false", "max", "if", "else", "while", "def", "val", "var", "match", "case", "class", "new", "null", "do")

  def apply(code: String): List[MiniScalaToken] =
    parse(tokens, code) match {
      case NoSuccess(_, next) => throw new SyntaxError(getPosition(next))
      case Success(result, _) => result
    }

  private def tokens: Parser[List[MiniScalaToken]] =
    phrase(
      rep1(
        leftParen |
          rightParen |
          leftBrace |
          rightBrace |
          arrow |
          literalBool |
          literalNull |
          iff |
          eelse |
          colon |
          comma |
          mmatch |
          ccase |
          op |
          eq |
          cclass |
          nnew |
          dot |
          simpleType |
          ddef |
          vval |
          vvar |
          wwhile |
          ddo |
          semicolon |
          identifier |
          literalString |
          literalFloat |
          literalInt
      )
    )

  private def identifier = positioned { "[a-zA-Z_][a-zA-Z0-9_]*".r ^? { case str if !keywords.contains(str) => IDENTIFIER(str) } }

  private def literalString = positioned { """"[^"]*"""".r ^^ { lit => STRING(lit.substring(1, lit.length - 1)) } }

  private def literalInt = positioned { """[0-9]+""".r ^^ { lit => INT(lit.toInt) } }

  private def literalFloat = positioned { """[0-9]+(\.[0-9]+)?f""".r ^^ { lit => FLOAT(lit.toFloat) } }

  private def literalBool = positioned { """(true|false)\b""".r ^^ { lit => BOOL(lit.toBoolean) } }

  private def literalNull = positioned { """null\b""".r ^^ { _ => NULL() } }

  private def op = positioned { """\+|\*|-|/|<=|==|<|%|!|\|\||\||&&|&|max""".r ^^ { lit => OP(lit) } }

  private def simpleType = positioned { """(String|Int|Float|Boolean|Unit|Null)\b""".r ^^ { lit => SIMPLE_TYPE(lit) } }

  private def leftParen = positioned { "(" ^^ { _ => LEFT_PAREN() } }

  private def rightParen = positioned { ")" ^^ { _ => RIGHT_PAREN() } }

  private def leftBrace = positioned { "{" ^^ { _ => LEFT_BRACE() } }

  private def rightBrace = positioned { "}" ^^ { _ => RIGHT_BRACE() } }

  private def iff = positioned { """if\b""".r ^^ { _ => IFF() } }

  private def eelse = positioned { """else\b""".r ^^ { _ => EELSE() } }

  private def ddo = positioned { """do\b""".r ^^ { _ => DO() } }

  private def wwhile = positioned { """while\b""".r ^^ { _ => WWHILE() } }

  private def eq = positioned { "=" ^^ { _ => EQ() } }

  private def colon = positioned { ":" ^^ { _ => COLON() } }

  private def comma = positioned { "," ^^ { _ => COMMA() } }

  private def semicolon = positioned { ";" ^^ { _ => SEMICOLON() } }

  private def ddef = positioned { """def\b""".r ^^ { _ => DDEF() } }

  private def vval = positioned { """val\b""".r ^^ { _ => VVAL() } }

  private def vvar = positioned { """var\b""".r ^^ { _ => VVAR() } }

  private def arrow = positioned { "=>" ^^ { _ => ARROW() } }

  private def mmatch = positioned { """match\b""".r ^^ { _ => MATCH() } }

  private def ccase = positioned { """case\b""".r ^^ { _ => CASE() } }

  private def cclass = positioned { """class\b""".r ^^ { _ => CLASS() } }

  private def dot = positioned { "." ^^ { _ => DOT() } }

  private def nnew = positioned { """new\b""".r ^^ { _ => NEW() } }

  private def getPosition(i: Lexer.Input) = SyntheticPosition(i.pos.line, i.pos.column)

  private case class SyntheticPosition(line: Int, column: Int, lineContents: String = "") extends Position
}
