package fr.emalios.algointerpreter.parser

import fr.emalios.algointerpreter.{parser, token}
import fr.emalios.algointerpreter.token._

import scala.util.parsing.combinator.Parsers

class TokensParser extends Parsers {

  override type Elem = Token

  private def parseString: Parser[parser.Literal] = {
    println("try to parse string")
    accept("string literal", { case string @ token.StringToken(value) => parser.StringLiteral(value) })
  }

  private def parseInteger: Parser[parser.Literal] = {
    println("try to parse integer")
    accept("integer literal", { case integer @ token.IntegerToken(value) => parser.Number(value) })
  }

  private def parseBoolean: Parser[parser.Literal] = {
    println("try to parse boolean")
    accept("boolean literal", { case boolean @ token.BooleanToken(value) => parser.BooleanLiteral(value)})
  }

  private def parseLiteral: Parser[Expression] = {
    println("try to parse literal")
    parseString | parseInteger | parseBoolean
  }

  private def parseIdentifier: Parser[parser.Identifier] = {
    accept("identifier", { case identifier @ token.Identifier(value) => parser.Identifier(value) })
  }

  private def parseUnary: Parser[Expression] = {
    println("try to parse unary")
    ( (Not | Minus ) ~ parseExpression) ^^ { case operator ~ expression => UnaryOperation(operator, expression) }
  }

  private def parseBinary: Parser[Expression] = {
    println("try to parse binary")
    chainl1(chainl1(parseGrouping | parseUnary | parseLiteral | parseBinary, parseMultiplication), parseAddition | parseSubtraction)
  }

  private def parseAddition: Parser[(Expression, Expression) => BinaryOperation] = {
    Plus ^^^ { (left: Expression, right: Expression) => (left, right) match { case (left: Expression, right: Expression) => BinaryOperation(left, Plus, right) } }
  }

  private def parseSubtraction: Parser[(Expression, Expression) => BinaryOperation] = {
    Minus ^^^ { (left: Expression, right: Expression) => (left, right) match { case (left: Expression, right: Expression) => BinaryOperation(left, Minus, right) } }
  }

  private def parseMultiplication: Parser[(Expression, Expression) => BinaryOperation] = {
    Mul ^^^ { (left: Expression, right: Expression) => (left, right) match { case (left: Expression, right: Expression) => BinaryOperation(left, Mul, right) } }
  }

  private def parseGrouping: Parser[Expression] = {
    println("try to parse grouping")
    LeftParen ~ parseExpression ~ RightParen ^^ { case _ ~ expression ~ _ => expression }
  }

  private def parseExpression: Parser[Expression] = {
    println("try to parse expression")
    parseUnary | parseBinary | parseLiteral | parseGrouping
  }

  /*
  private def parseIfThenInstruction(): Parser[IfThenElseInstruction] = {
    ( If ~ parseExpression ~ Then ~ parseBlock(Then, EndIf) ~ EndIf) ^^ { case _ ~ condition ~ _ ~ block ~ _ => IfThenElseInstruction(condition, block, Option.empty) }
  }

  private def parseIfThenElseInstruction(): Parser[IfThenElseInstruction] = {
    ( If ~ parseExpression ~ Then ~ parseBlock(Then, Else) ~ Else ~ parseBlock(Else, EndIf) ~ EndIf) ^^ { case _ ~ condition ~ _ ~ thenBlock ~ _ ~ elseBlock ~ _ => IfThenElseInstruction(condition, thenBlock, Option(elseBlock)) }
  }

   */
  /*
  private def parseForInstruction: Parser[Instruction] = {
    ( For ~ parseIdentifier ~ From ~ parseExpression ~ To ~ parseExpression ~ parseBlock(Do, EndFor) ) ^^ { case _ ~ identifier ~ _ ~ expressionFrom ~ _ ~ expressionTo ~ block => ForInstruction(identifier, expressionFrom, expressionTo, block)}
  }

   */

  private def parseBlock(startDelimiter: Token, endDelimiter: Token): Parser[Block] = {
    startDelimiter ~> rep1(parseInstruction) <~ endDelimiter ^^ Block
  }

  private def parseAffectation: Parser[parser.Affectation] = {
    ( parseIdentifier ~ Affectation ~ parseExpression ) ^^ { case identifier ~ _ ~ expression => parser.Affectation(identifier, expression)}
  }

  private def parseInstruction: Parser[Instruction] = {
    /*parseIfThenInstruction() | parseIfThenElseInstruction() | parseForInstruction  | */ parseAffectation
  }

  private def parseAlgo: Parser[Algo] = {
    phrase(parseBlock(Start, End)) ^^ Algo
  }

  def apply(tokens: Seq[Token]): Algo = {
    val reader = new TokenReader(tokens)
    parseAlgo.apply(reader) match {
      case NoSuccess(msg, _) => println(msg); null
      case Success(result, _) => result
    }
  }

}
