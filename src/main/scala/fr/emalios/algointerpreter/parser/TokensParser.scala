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

  private def parseIdentifier: Parser[parser.Identifier] = {
    accept("identifier", { case identifier @ token.Identifier(value) => parser.Identifier(value) })
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

  private def parseExpression: Parser[Expression] = {
    parseLiteral | parseUnary | parseBinary
  }

  /*
  private def parseIfThenInstruction(): Parser[IfThenElseInstruction] = {
    ( If ~ parseExpression ~ Then ~ parseBlock(Then, EndIf) ~ EndIf) ^^ { case _ ~ condition ~ _ ~ block ~ _ => IfThenElseInstruction(condition, block, Option.empty) }
  }

  private def parseIfThenElseInstruction(): Parser[IfThenElseInstruction] = {
    ( If ~ parseExpression ~ Then ~ parseBlock(Then, Else) ~ Else ~ parseBlock(Else, EndIf) ~ EndIf) ^^ { case _ ~ condition ~ _ ~ thenBlock ~ _ ~ elseBlock ~ _ => IfThenElseInstruction(condition, thenBlock, Option(elseBlock)) }
  }

   */

  private def parseForInstruction: Parser[Instruction] = {
    ( For ~ parseIdentifier ~ From ~ parseExpression ~ To ~ parseExpression ~ parseBlock(Do, EndFor) ) ^^ { case _ ~ identifier ~ _ ~ expressionFrom ~ _ ~ expressionTo ~ block => ForInstruction(identifier, expressionFrom, expressionTo, block)}
  }

  private def parseBlock(startDelimiter: Token, endDelimiter: Token): Parser[Block] = {
    startDelimiter ~> rep1(parseInstruction) <~ endDelimiter ^^ Block
  }

  private def parseAffectation: Parser[parser.Affectation] = {
    ( parseIdentifier ~ Affectation ~ parseExpression ) ^^ { case identifier ~ _ ~ expression => parser.Affectation(identifier, expression)}
  }

  private def parseInstruction: Parser[Instruction] = {
    /*parseIfThenInstruction() | parseIfThenElseInstruction() | */parseForInstruction | parseAffectation
  }

  private def parseAlgo: Parser[AlgoAST] = {
    (phrase(parseBlock(Start, End))) ^^ Algo
  }

  def keyword(kw: String): Parser[Token] =
    accept(kw, {
      case (Start) => Start
      case (End) => End
      case (For) => For
      case (From) => From
      case (To) => To
      case (Affectation) => Affectation
      case EndIf => EndIf
      case EndFor => EndFor
      case While => While
      case EndWhile => EndWhile
      case RightParen => RightParen
      case LeftParen => LeftParen
      // ...
    })

  def operator(kw: String): Parser[Operator] =
    accept(kw, {
      case (Not) => Not
      case (Minus) => Minus
      case Plus => Plus
      case Minus => Minus
      case Slash => Slash
      // ...
    })

  def apply(tokens: Seq[Token]): AlgoAST = {
    val reader = new TokenReader(tokens)
    parseAlgo.apply(reader) match {
      case NoSuccess(msg, _) => println(msg); null
      case Success(result, _) => result
    }
  }

}
