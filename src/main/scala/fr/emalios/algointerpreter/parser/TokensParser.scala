package fr.emalios.algointerpreter.parser

import fr.emalios.algointerpreter.Main.devDebugMode
import fr.emalios.algointerpreter.eval.Quantifier
import fr.emalios.algointerpreter.{parser, token}
import fr.emalios.algointerpreter.token._
import fr.emalios.algointerpreter.typecheck.{BooleanType, CharType, FunctionType, IntegerType, RealType, StringType, Type}

import scala.util.parsing.combinator.Parsers

class TokensParser extends Parsers {

  override type Elem = Token

  private def parseString: Parser[parser.Literal] = {
    if(devDebugMode) println("try to parse string")
    accept("string literal", { case string @ token.StringToken(value) => parser.StringLiteral(value) })
  }

  private def parseInteger: Parser[parser.Literal] = {
    if (devDebugMode) println("try to parse integer")
    accept("integer literal", { case integer @ token.IntegerToken(value) => parser.Number(value) })
  }

  private def parseBoolean: Parser[parser.Literal] = {
    if(devDebugMode) println("try to parse boolean")
    accept("boolean literal", { case boolean @ token.BooleanToken(value) => parser.BooleanLiteral(value)})
  }

  private def parseLiteral: Parser[Expression] = {
    if (devDebugMode) println("try to parse literal")
    parseString | parseInteger | parseBoolean
  }

  private def parseIdentifier: Parser[parser.Identifier] = {
    if(devDebugMode) println("try to parse identifier")
    accept("identifier", { case identifier @ token.Identifier(value) => parser.Identifier(value) })
  }

  private def parseUnary: Parser[Expression] = {
    if (devDebugMode) println("try to parse unary")
    ( (Not | Minus ) ~ parseExpression) ^^ { case operator ~ expression => UnaryOperation(operator, expression) }
  }

  private def parseBinary: Parser[Expression] = {
    if (devDebugMode) println("try to parse binary")
    chainl1(chainl1(chainl1(parseGrouping | parseUnary | parseLiteral | parseFunctionCall | parseIdentifier, parseMultiplication | parseDivision | parseMod | parseEuclideanDivision | parseBooleanExpr), parseAddition | parseSubtraction), parseInequality | parseEquality)
  }

  private def parseBooleanExpr: Parser[(Expression, Expression) => BinaryOperation] = {
    parseAnd | parseOr | parseLesserEquals | parseLesser | parseGreaterEquals | parseGreater
  }

  private def parseMod: Parser[(Expression, Expression) => BinaryOperation] = {
    Mod ^^^ { (left: Expression, right: Expression) => (left, right) match { case (left: Expression, right: Expression) => BinaryOperation(left, Mod, right) } }
  }

  private def parseDivision: Parser[(Expression, Expression) => BinaryOperation] = {
    Slash ^^^ { (left: Expression, right: Expression) => (left, right) match { case (left: Expression, right: Expression) => BinaryOperation(left, Slash, right) } }
  }

  private def parseEuclideanDivision: Parser[(Expression, Expression) => BinaryOperation] = {
    Percent ^^^ { (left: Expression, right: Expression) => (left, right) match { case (left: Expression, right: Expression) => BinaryOperation(left, Percent, right) } }
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

  private def parseInequality: Parser[(Expression, Expression) => BinaryOperation] = {
    NotEquals ^^^ { (left: Expression, right: Expression) => (left, right) match {
      case (left: Expression, right: Expression) => BinaryOperation(left, NotEquals, right)
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
    if (devDebugMode) println("try to parse grouping")
    LeftParen ~ parseExpression ~ RightParen ^^ { case _ ~ expression ~ _ => expression }
  }

  private def parseExpression: Parser[Expression] = {
    if (devDebugMode) println("try to parse expression")
    parseFunctionCall | parseUnary | parseBinary | parseLiteral | parseGrouping
  }

  private def parseExprFunctionCall: Parser[ExprInstr] = {
    if (devDebugMode) println("try to parse expr function")
    (parseIdentifier ~ parseArgs) ^^ { case identifier ~ args => ExprInstr(FunctionCall(identifier, args))}
  }

  private def parseFunctionCall: Parser[FunctionCall] = {
    if (devDebugMode) println("try to parse function")
    (parseIdentifier ~ parseArgs) ^^ { case identifier ~ args => FunctionCall(identifier, args)}
  }

  private def parseArgs: Parser[List[Expression]] = {
    if (devDebugMode) println("try to parse args")
    LeftParen ~> repsep(parseExpression, Comma) <~ RightParen
  }

  private def parseBlock(startDelimiter: Parser[Token], endDelimiter: Parser[Token]): Parser[Block] = {
    if (devDebugMode) println("try to parse block")
    startDelimiter ~> rep1(parseInstruction) <~ endDelimiter ^^ Block
  }

  private def parseAffectation: Parser[parser.Assignment] = {
    if (devDebugMode) println("try to parse affectation")
    ( parseIdentifier ~ Affectation ~ parseExpression ) ^^ { case identifier ~ _ ~ expression => parser.Assignment(identifier, expression)}
  }

  private def parseReturn: Parser[parser.Return] = {
    if (devDebugMode) println("try to parse return")
    ( Return ~> parseExpression ) ^^ parser.Return
  }

  private def parseInstruction: Parser[Instruction] = {
    if (devDebugMode) println("try to parse instruction")
    (parseIfThenElseInstruction | parseForInstruction | parseWhileInstruction | parseReturn | parseExprFunctionCall | parseAffectation) <~ parseEndOfLine
  }

  private def parseForInstruction: Parser[ForInstruction] = {
    (For ~> parseIdentifier <~ From) ~ (parseExpression <~ To) ~ (parseExpression <~ (Do ~ parseEndOfLine)) ~ ((parseInstruction*) <~ EndFor) ^^ { case identifier ~ from ~ to ~ block => ForInstruction(identifier, from, to, Block(block)) }
  }

  private def parseWhileInstruction: Parser[WhileInstruction] = {
    (While ~> parseExpression <~ (Do ~ parseEndOfLine)) ~ (parseInstruction*) <~ EndWhile ^^ { case condition ~ block => WhileInstruction(condition, Block(block)) }
  }

  private def parseIfThenElseInstruction: Parser[IfThenElseInstruction] = {
    (If ~> parseExpression <~ (Then ~ parseEndOfLine)) ~ (parseInstruction*) ~ (opt((Else ~ opt(parseEndOfLine)) ~> parseInstruction*) <~ EndIf) ^^ { case expression ~ thenBlock ~ elseBlock => IfThenElseInstruction(expression, Block(thenBlock), Option(Block(elseBlock.get))) }
  }

  private def parseFunction: Parser[parser.Function] = {
    if (devDebugMode) println("try to parse function")
    parseFunctionDeclaration ~ parseEndOfLine ~ parseAlgo ^^ { case functionDeclaration ~ _ ~ algo => parser.Function(functionDeclaration, algo) }
  }

  private def parseFunctionDeclaration: Parser[FunctionDeclaration] = {
    if (devDebugMode) println("try to parse function declaration")
    (Function ~> parseIdentifier <~ LeftParen) ~ opt(parseTypeParameters) ~ (RightParen ~> opt(parseReturnType)) ^^ { case identifier ~ typeParameters ~ returnType => FunctionDeclaration(identifier, FunctionType(typeParameters, returnType)) }
  }

  private def parseReturnType: Parser[Type] = {
    if (devDebugMode) println("try to parse return type")
    DoublePoints ~> parseType
  }

  private def parseTypeParameters: Parser[List[TypeParameter]] = {
    if (devDebugMode) println("try to parse list of type parameter")
      repsep(parseTypeParameter, Comma)
  }

  private def parseTypeParameter: Parser[TypeParameter] = {
    if (devDebugMode) println("try to parse type parameter")
    (parseIdentifier ~ opt(parseQuantifier) ~ DoublePoints ~ parseType) ^^ { case identifier ~ quantifier ~ _ ~ paramType => quantifier match {
      case Some(_) => TypeParameter(identifier, paramType, quantifier.get)
      case None =>TypeParameter(identifier, paramType, fr.emalios.algointerpreter.eval.In)
    } }
  }

  private def parseQuantifier: Parser[Quantifier] = {
    InOut ^^^ fr.emalios.algointerpreter.eval.InOut |
    In ^^^ fr.emalios.algointerpreter.eval.In       |
    Out ^^^ fr.emalios.algointerpreter.eval.Out
  }

  private def parseType: Parser[Type] = {
    if (devDebugMode) println("try to parse type")
        CharTypeToken ^^^ CharType       |
        BooleanTypeToken ^^^ BooleanType |
        IntegerTypeToken ^^^ IntegerType |
        StringTypeToken ^^^ StringType   |
        RealTypeToken ^^^ RealType
  }

  private def parseAlgo: Parser[Algo] = {
    if (devDebugMode) println("try to parse algo")
    parseBlock(Start <~ parseEndOfLine, End) ^^ Algo
  }

  private def parseProgram: Parser[Program] = {
    if(devDebugMode) println("try to parse program")
    parseAlgo ~ opt(parseEndOfLine) ~ repsep(parseFunction, parseEndOfLine) ^^ { case mainAlgo ~ _ ~ functions => Program(mainAlgo, functions) }
  }

  private def parseEndOfLine: Parser[Token] = {
    if (devDebugMode) println("try to parse end of line")
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
