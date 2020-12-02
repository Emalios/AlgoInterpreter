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
    (Not ~ ) ^^ { case a ~ b => UnaryExpression(a, b) }
  }

  private def parseExpression(): Parser[Expression] = {
    parseLiteral | parseUnary
  }

  /*
  private def parseIfThenElseInstruction: Parser[parser.IfThenElseInstruction] = {
      If ~ expression ~ Then ~ Block ^^ { case _ ~ condition ~ _ ~ thenBlock ~ _ ~ elseBlock => IfThenElseInstruction(condition, thenBlock, elseBlock)
  }

   */



  /*
  def parseExpression: Parser[Expression] = {
    parseLiteral ^^ { case id @ parser.Literal(_) => id} |

  }

   */

  def keyword(kw: String): Parser[Token] =
    accept(kw, {
      case (Start) => Start
      case (End) => End
      case (For) => For
      case (From) => From
      case (To) => To
      // ...
    })

  /*
  def apply(tokens: Seq[Token]): Either[AlgoLexerError, AlgoAST] = {
    val reader = new TokenReader(tokens)
    parse(keyword(), reader) match {
      case NoSuccess(msg, next) => Left(AlgoLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }

   */

}
