package fr.emalios.algointerpreter.parser

import fr.emalios.algointerpreter.Main.debugMode
import fr.emalios.algointerpreter.eval.{CharType, IntegerType, RealType, StringType, Type}
import fr.emalios.algointerpreter.{parser, token}
import fr.emalios.algointerpreter.token._

import scala.util.parsing.combinator.Parsers

class TokensParser extends Parsers {

  override type Elem = Token

  private def parseString: Parser[parser.Literal] = {
    if(debugMode) println("try to parse string")
    accept("string literal", { case string @ token.StringToken(value) => parser.StringLiteral(value) })
  }

  private def parseInteger: Parser[parser.Literal] = {
    if (debugMode) println("try to parse integer")
    accept("integer literal", { case integer @ token.IntegerToken(value) => parser.Number(value) })
  }

  private def parseBoolean: Parser[parser.Literal] = {
    if(debugMode) println("try to parse boolean")
    accept("boolean literal", { case boolean @ token.BooleanToken(value) => parser.BooleanLiteral(value)})
  }

  private def parseLiteral: Parser[Expression] = {
    if (debugMode) println("try to parse literal")
    parseString | parseInteger | parseBoolean
  }

  private def parseIdentifier: Parser[parser.Identifier] = {
    if(debugMode) println("try to parse identifier")
    accept("identifier", { case identifier @ token.Identifier(value) => parser.Identifier(value) })
  }

  private def parseUnary: Parser[Expression] = {
    if (debugMode) println("try to parse unary")
    ( (Not | Minus ) ~ parseExpression) ^^ { case operator ~ expression => UnaryOperation(operator, expression) }
  }

  private def parseBinary: Parser[Expression] = {
    if (debugMode) println("try to parse binary")
    chainl1(chainl1(chainl1(parseGrouping | parseUnary | parseLiteral | parseIdentifier, parseMultiplication | parseBooleanExpr), parseAddition | parseSubtraction), parseEquality)
  }

  private def parseBooleanExpr: Parser[(Expression, Expression) => BinaryOperation] = {
    parseAnd | parseOr | parseLesser | parseLesserEquals | parseGreater | parseGreaterEquals
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

  private def parseEquality: Parser[(Expression, Expression) => BinaryOperation] = {
    Equals ^^^ { (left: Expression, right: Expression) => (left, right) match {
      case (left: Expression, right: Expression) => BinaryOperation(left, Equals, right)
    }}
  }

  private def parseAnd: Parser[(Expression, Expression) => BinaryOperation] = {
    And ^^^ { (left: Expression, right: Expression) => (left, right) match {
      case (left: Expression, right: Expression) => BinaryOperation(left, And, right)
    }}
  }

  private def parseOr: Parser[(Expression, Expression) => BinaryOperation] = {
    Or ^^^ { (left: Expression, right: Expression) => (left, right) match {
      case (left: Expression, right: Expression) => BinaryOperation(left, Or, right)
    }}
  }

  private def parseLesser: Parser[(Expression, Expression) => BinaryOperation] = {
    Lesser ^^^ { (left: Expression, right: Expression) => (left, right) match {
      case (left: Expression, right: Expression) => BinaryOperation(left, Lesser, right)
    }}
  }

  private def parseLesserEquals: Parser[(Expression, Expression) => BinaryOperation] = {
    LesserEqual ^^^ { (left: Expression, right: Expression) => (left, right) match {
      case (left: Expression, right: Expression) => BinaryOperation(left, LesserEqual, right)
    }}
  }

  private def parseGreater: Parser[(Expression, Expression) => BinaryOperation] = {
    Greater ^^^ { (left: Expression, right: Expression) => (left, right) match {
      case (left: Expression, right: Expression) => BinaryOperation(left, Greater, right)
    }}
  }

  private def parseGreaterEquals: Parser[(Expression, Expression) => BinaryOperation] = {
    GreaterEqual ^^^ { (left: Expression, right: Expression) => (left, right) match {
      case (left: Expression, right: Expression) => BinaryOperation(left, GreaterEqual, right)
    }}
  }

  private def parseGrouping: Parser[Expression] = {
    if (debugMode) println("try to parse grouping")
    LeftParen ~ parseExpression ~ RightParen ^^ { case _ ~ expression ~ _ => expression }
  }

  private def parseExpression: Parser[Expression] = {
    if (debugMode) println("try to parse expression")
    parseFunctionCall | parseUnary | parseBinary | parseLiteral | parseGrouping
  }

  private def parseExprFunctionCall: Parser[ExprInstr] = {
    if (debugMode) println("try to parse expr function")
    (parseIdentifier ~ parseArgs) ^^ { case identifier ~ args => ExprInstr(FunctionCall(identifier, args))}
  }

  private def parseFunctionCall: Parser[FunctionCall] = {
    if (debugMode) println("try to parse function")
    (parseIdentifier ~ parseArgs) ^^ { case identifier ~ args => FunctionCall(identifier, args)}
  }

  private def parseArgs: Parser[List[Expression]] = {
    if (debugMode) println("try to parse args")
    LeftParen ~> repsep(parseExpression, Comma) <~ RightParen
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

  private def parseBlock(startDelimiter: Parser[Token], endDelimiter: Parser[Token]): Parser[Block] = {
    if (debugMode) println("try to parse block")
    startDelimiter ~> rep1(parseInstruction) <~ endDelimiter ^^ Block
  }

  private def parseAffectation: Parser[parser.Affectation] = {
    if (debugMode) println("try to parse affectation")
    ( parseIdentifier ~ Affectation ~ parseExpression ) ^^ { case identifier ~ _ ~ expression => parser.Affectation(identifier, expression)}
  }

  private def parseInstruction: Parser[Instruction] = {
    if (debugMode) println("try to parse instruction")
    /*parseIfThenInstruction() | parseIfThenElseInstruction() | parseForInstruction  | */ (parseAffectation | parseExprFunctionCall) <~ EndOfLine
  }

  private def parseFunction: Parser[parser.Function] = {
    if (debugMode) println("try to parse function")
    parseFunctionDeclaration ~ parseEndOfLine ~ parseAlgo ^^ { case functionDeclaration ~ _ ~ algo => parser.Function(functionDeclaration, algo) }
  }

  private def parseFunctionDeclaration: Parser[FunctionDeclaration] = {
    if (debugMode) println("try to parse function declaration")
    Function ~> parseIdentifier ~ parseTypeParameters ~  opt(parseReturnType) ^^ { case identifier ~ typeParameters ~ returnType => FunctionDeclaration(identifier, typeParameters, returnType) }
  }

  private def parseReturnType: Parser[Type] = {
    if (debugMode) println("try to parse return type")
    DoublePoints ~> parseType
  }

  private def parseTypeParameters: Parser[List[TypeParameter]] = {
    if (debugMode) println("try to parse list of type parameter")
      LeftParen ~> repsep(parseTypeParameter, Comma) <~ RightParen
  }

  private def parseTypeParameter: Parser[TypeParameter] = {
    if (debugMode) println("try to parse type parameter")
    (parseIdentifier ~ DoublePoints ~ parseType) ^^ { case identifier ~ _ ~ paramType => TypeParameter(identifier, paramType) }
  }

  private def parseType: Parser[Type] = {
    if (debugMode) println("try to parse type")
        CharTypeToken ^^^ CharType |
        IntegerTypeToken ^^^ IntegerType |
        StringTypeToken ^^^ StringType |
        RealTypeToken ^^^ RealType
  }

  private def parseAlgo: Parser[Algo] = {
    if (debugMode) println("try to parse algo")
    parseBlock(Start <~ parseEndOfLine, End) ^^ Algo
  }

  private def parseProgram: Parser[Program] = {
    if(debugMode) println("try to parse program")
    parseAlgo ~ parseEndOfLine ~ repsep(parseFunction, rep1(parseEndOfLine)) ^^ { case mainAlgo ~ _ ~ functions => Program(mainAlgo, functions) }
  }

  private def parseEndOfLine: Parser[Token] = {
    if (debugMode) println("try to parse end of line")
    rep1(EndOfLine) ^^^ EndOfLine
  }

  def apply(tokens: Seq[Token]): Program = {
    val reader = new TokenReader(tokens)
    parseProgram.apply(reader) match {
      case NoSuccess(msg, _) => println(msg); null
      case Success(result, _) => result
    }
  }

  def applyInput(tokens: Seq[Token]): Expression = {
    val reader = new TokenReader(tokens)
    parseExpression.apply(reader) match {
      case NoSuccess(msg, _) => println(msg); null
      case Success(result, _) => result
    }
  }

}
