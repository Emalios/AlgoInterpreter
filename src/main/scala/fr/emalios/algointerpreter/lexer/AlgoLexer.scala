package fr.emalios.algointerpreter.lexer

import fr.emalios.algointerpreter.token._

import scala.collection.immutable
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class AlgoLexer extends RegexParsers {

  /**
   * Method that tells the parser that we want to ignore characters defined in whiteSpace.
   * @return true
   */
  override def skipWhitespace = true

  /**
   * Redefinition of the regex which matches the characters that we want to pass,
   * here we want to ignore all the whitespaces because in algo we don't care about whitespaces.
   */
  override val whiteSpace: Regex = "[ \n\t\r\f]+".r

  /**
   * The value is matched here if it starts with a '"' and finish by '"'.
   * As an example, for an entry `name <- "Hugo"`,
   * the method will return a parser which associates "Hugo" with LITERAL token which wraps "Hugo".
   * @return parser which associates a value which is matched by the regex with a Token which wraps this value.
   */
  def literal: Parser[Literal] = {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      Literal(content)
    }
  }

  /**
   * The value is matched here if it starts with a digit and contains digits.
   * As an example, for an entry `number <- 42`,
   * the method will return a parser which associates 42 with NUMBER token which wraps 42.
   * @return parser which associates a value which is matched by the regex with a Token which wraps this value.
   */
  def number: Parser[Number] = {
    "[0-9]+".r ^^ { str =>
      Number(Integer.parseInt(str))
    }
  }

  val keywords:immutable.HashMap[String, Token] = immutable.HashMap(
    "Fin" -> End,
    "Debut" -> Start,
    "si"-> If ,
    "fsi" -> EndIf,
    "et" -> AND,
    "ou" -> OR,
    "Pour" -> For,
    "de" -> From,
    "a" -> To,
    "fpour" -> EndFor,
    "tantque" -> While,
    "ftant" -> EndWhile,
    "alors" -> Then,
    "sinon" -> Else,
    "faire" -> Do,
    "fonction" -> Function,
    "mod" -> Mod,
    "retourne" -> RETURN
  )

  /**
   * The value is matched here if it starts with a letter and contains letters, numbers or the character '_'.
   * As an example, for an entry "number <- 42",
   * the method will return a parser which associates "number" with IDENTIFIER token which wraps "number".
   * @return parser which associates a value which is matched by the regex with a Token which wraps this value.
   */
  def identifierOrKeyword: Parser[Token] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => if(keywords.contains(str)) keywords(str) else Identifier(str) }
  }

  /** TODO: rewrite
   * As we have a finite list of tokens, we assign each token to its literal value
   * @return parser which associates a token with to its literal value.
   */
  def operator: Parser[Operator] = {
    "<-" ^^ {_ => AFFECTATION} | "=" ^^ {_ => EQUALS} | "(" ^^ {_ => RightParen} | ")" ^^ { _ => LeftParen} | "+" ^^ { _ => PLUS}
  }

  /**
   * Method that will create a sentence (set of tokens)
   * @return a set of tokens
   */
  def tokens: Parser[List[Token]] = {
    phrase(rep1(identifierOrKeyword | number | operator))
  }

  def apply(code: String): Either[AlgoLexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(AlgoLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }

}
