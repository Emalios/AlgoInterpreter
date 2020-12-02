package fr.emalios.algointerpreter.parser

import fr.emalios.algointerpreter.Main.keywords
import fr.emalios.algointerpreter.lexer.AlgoLexerError
import fr.emalios.algointerpreter.{parser, token}
import fr.emalios.algointerpreter.token._

import scala.util.parsing.combinator.{Parsers, RegexParsers}

class TokensParser extends Parsers {

  override type Elem = Token

  private def parseLiteral: Parser[Expression] = {
    accept("literal expression", { case literal @ token.Literal(name) => parser.Literal(name) })
  }

  private def parseUnary: Parser[Expression] = {
    ( (operator("non") | operator("-") ) ~ parseExpression) ^^ { case operator ~ expression => UnaryOperation(operator, expression) }
  }

  private def parseBinary: Parser[Expression] = {
    ( parseExpression ~ (
          operator("-") |
          operator("+") |
          operator("mod") |
          operator("/") |
          operator("%") |
          operator(">") |
          operator(">=") |
          operator("<") |
          operator("<=") |
          operator("=") |
          operator("*") |
          operator("et") |
          operator("ou")) ~ parseExpression) ^^ { case left ~ operator ~ right => BinaryOperation(left, operator, right) }
  }

  private def parseExpression(): Parser[Expression] = {
    parseLiteral | parseUnary | parseBinary
  }

  def keyword(kw: String): Parser[Token] =
    accept(kw, {
      case (Start) => Start
      case (End) => End
      case (For) => For
      case (From) => From
      case (To) => To
      // ...
    })

  def operator(kw: String): Parser[Operator] =
    accept(kw, {
      case (Not) => Not
      case (Minus) => Minus
      // ...
    })

}
