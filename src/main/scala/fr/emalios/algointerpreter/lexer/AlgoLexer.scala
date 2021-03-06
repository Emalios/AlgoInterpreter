package fr.emalios.algointerpreter.lexer

import fr.emalios.algointerpreter.parser.StringLiteral
import fr.emalios.algointerpreter.token
import fr.emalios.algointerpreter.token.EndOfLine
import fr.emalios.algointerpreter.token._

import scala.collection.immutable
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class AlgoLexer extends RegexParsers {

  /**
   * Method that tells the parser that we want to ignore characters defined in whiteSpace.
   *
   * @return true
   */
  override def skipWhitespace = true

  /**
   * Redefinition of the regex which matches the characters that we want to pass,
   * here we want to ignore all the whitespaces because in algo we don't care about whitespaces.
   */
  override val whiteSpace: Regex = "[^\\S\r\n]+".r

  /**
   * The value is matched here if it starts with a '"' and finish by '"'.
   * As an example, for an entry `name <- "Hugo"`,
   * the method will return a parser which associates "Hugo" with Literal token which wraps "Hugo".
   *
   * @return parser which associates a value which is matched by the regex with a Token which wraps this value.
   */
  def stringLit: Parser[StringToken] = {
    """"[^"]*"""".r ^^ { str => val content = str.substring(1, str.length - 1)
      token.StringToken(content) }
  }

  /**
   * The value is matched here if it starts with a digit and contains digits.
   * As an example, for an entry `number <- 42`,
   * the method will return a parser which associates 42 with Number token which wraps 42.
   *
   * @return parser which associates a value which is matched by the regex with a Token which wraps this value.
   */
  def numberLit: Parser[IntegerToken] = {
    "[0-9]+".r ^^ { str =>
      IntegerToken(Integer.parseInt(str))
    }
  }

  val keywords: immutable.HashMap[String, Token] = immutable.HashMap(
    "entier" -> IntegerTypeToken,
    "booleen" -> BooleanTypeToken,
    "reel" -> RealTypeToken,
    "chaine" -> StringTypeToken,
    "char" -> CharTypeToken,
    "In" -> In,
    "Out" -> Out,
    "InOut" -> InOut,
    "Fin" -> End,
    "Debut" -> Start,
    "si" -> If,
    "fsi" -> EndIf,
    "pour" -> For,
    "de" -> From,
    "a" -> To,
    "fpour" -> EndFor,
    "tantque" -> While,
    "ftant" -> EndWhile,
    "alors" -> Then,
    "sinon" -> Else,
    "faire" -> Do,
    "fonction" -> Function,
    "retourne" -> Return,
    "vrai" -> BooleanToken(true),
    "faux" -> BooleanToken(false)
  )

  /**
   * The value is matched here if it starts with a letter and contains letters, numbers or the character '_'.
   * As an example, for an entry "number <- 42",
   * the method will return a parser which associates "number" with Identifier token which wraps "number".
   *
   * @return parser which associates a value which is matched by the regex with a Token which wraps this value.
   */
  def identifierOrKeyword: Parser[Token] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str =>
      if (keywords.contains(str)) keywords(str) else Identifier(str)
    }
  }

  /**
   * @return parser which associates a end of line character matched by the regex with the EndOfLine token.
   */
  def parseEndOfLine: Parser[Token] = {
    "\r?\n".r ^^^ EndOfLine
  }

  def operator: Parser[Token] = {
    (
      "<-" ^^^ Affectation
        | "mod" ^^^ Mod
        | "=" ^^^ Equals
        | "%" ^^^ Percent
        | ":" ^^^ DoublePoints
        | "+" ^^^ Plus
        | "-" ^^^ Minus
        | "*" ^^^ Mul
        | "/" ^^^ Slash
        | "<=" ^^^ LesserEqual
        | ">=" ^^^ GreaterEqual
        | "<" ^^^ Less
        | ">" ^^^ Greater
        | "et" ^^^ And
        | "ou" ^^^ Or
        | "," ^^^ Comma
        | "(" ^^^ LeftParen
        | ")" ^^^ RightParen
        | "!=" ^^^ NotEquals
        | "!" ^^^ Not
      )
  }

  /**
   * Method that will create a sentence (set of tokens)
   * @return a set of tokens
   */
  def tokens: Parser[List[Token]] = {
    phrase(rep1(operator | identifierOrKeyword | parseEndOfLine | stringLit | numberLit))
  }

  def apply(code: String): List[Token] = {
    parse(tokens, code) match {
      case NoSuccess(msg, _) => throw AlgoLexerError(msg)
      case Success(result, _) => result
    }
  }

}
