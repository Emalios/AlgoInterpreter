package fr.emalios.algointerpreter.lexer

import fr.emalios.algointerpreter.parser.StringLiteral
import fr.emalios.algointerpreter.token
import fr.emalios.algointerpreter.token.EndOfLine
import fr.emalios.algointerpreter.token._

import scala.collection.immutable
import scala.language.postfixOps
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
    //println("string lit")
    (""""[^"]*"""".r ^^ { str => val content = str.substring(1, str.length - 1); token.StringToken(content) }).withFailureMessage(s"chaîne attendu")
  }

  /**
   * The value is matched here if it starts with a digit and contains digits.
   * As an example, for an entry `number <- 42`,
   * the method will return a parser which associates 42 with Number token which wraps 42.
   *
   * @return parser which associates a value which is matched by the regex with a Token which wraps this value.
   */
  def numberLit: Parser[IntegerToken] = {
    //println("number lit")
    ("[0-9]+".r ^^ { str => IntegerToken(Integer.parseInt(str))}).withFailureMessage("Nombre entier attendu")
  }

  val keywords: immutable.HashMap[String, Token] = immutable.HashMap(
    IntegerTypeToken.toString -> IntegerTypeToken,
    BooleanTypeToken.toString -> BooleanTypeToken,
    RealTypeToken.toString -> RealTypeToken,
    StringTypeToken.toString -> StringTypeToken,
    CharTypeToken.toString -> CharTypeToken,
    In.toString -> In,
    Out.toString -> Out,
    InOut.toString -> InOut,
    End.toString -> End,
    Start.toString -> Start,
    If.toString -> If,
    EndIf.toString -> EndIf,
    For.toString -> For,
    From.toString -> From,
    To.toString -> To,
    EndFor.toString -> EndFor,
    While.toString -> While,
    EndWhile.toString -> EndWhile,
    Then.toString -> Then,
    Else.toString -> Else,
    Do.toString -> Do,
    Function.toString -> Function,
    Return.toString -> Return,
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
    //println("identifier or keyword")
    ("[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => if (keywords.contains(str)) keywords(str) else Identifier(str)}).withFailureMessage("Identifieur ou mot clé attendu")
  }

  /**
   * @return parser which associates a end of line character matched by the regex with the EndOfLine token.
   */
  def parseEndOfLine: Parser[Token] = {
    //println("end of line")
    "\r?\n".r ^^^ {EndOfLine}
  }

  def operator: Parser[Token] = {
    //println("operator")
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
        | "(" ^^^ {LeftParen}
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
    phrase(rep1(operator | identifierOrKeyword | parseEndOfLine | stringLit | numberLit)).withFailureMessage("Erreur de syntaxe")
  }

  def apply(code: String): List[Token] = {
    parse(tokens, code) match {
      case NoSuccess(msg, reste: Input) => throw AlgoLexerError(msg, reste)
      case Success(result, _) => result
    }
  }

}
