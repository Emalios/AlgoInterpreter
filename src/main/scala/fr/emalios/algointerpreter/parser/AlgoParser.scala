package fr.emalios.algointerpreter.parser

import fr.emalios.algointerpreter.Main.devDebugMode
import fr.emalios.algointerpreter.eval.Quantifier
import fr.emalios.algointerpreter.{parser, token}
import fr.emalios.algointerpreter.token._
import fr.emalios.algointerpreter.typecheck.algow.{BooleanType, CharType, FunctionType, IntegerType, RealType, StringType, Type, UnitType}

import scala.language.postfixOps
import scala.util.parsing.combinator.Parsers

class AlgoParser extends Parsers {

  override type Elem = Token

  private def parseString: Parser[parser.Literal] = {
    if(devDebugMode) println("try to parse string")
    accept("string literal", { case StringToken(value) => StringLiteral(value) })
  }

  private def parseInteger: Parser[parser.Literal] = {
    if (devDebugMode) println("try to parse integer")
    accept("integer literal", { case IntegerToken(value) => Number(value) })
  }

  private def parseBoolean: Parser[parser.Literal] = {
    if(devDebugMode) println("try to parse boolean")
    accept("boolean literal", { case BooleanToken(value) => BooleanLiteral(value)})
  }

  private def parseLiteral: Parser[Expression] = {
    if (devDebugMode) println("try to parse literal")
    parseString | parseInteger | parseBoolean
  }

  private def parseIdentifier: Parser[parser.Identifier] = {
    if(devDebugMode) println("try to parse identifier")
    accept("identifier", { case token.Identifier(value) => parser.Identifier(value) })
  }

  private def parseUnary: Parser[Expression] = {
    if (devDebugMode) println("try to parse unary")
    ( (Not | Minus) ~ parseExpression) ^^ { case operator ~ expression => operator match {
      case operator: Operator => operator match {
        case operator: UnaryOperator => UnaryOperation(operator, expression)
      }
    } }
  }

  private def parseBinary: Parser[Expression] = {
    if (devDebugMode) println("try to parse binary")
    chainl1(chainl1(chainl1(parseGrouping | parseUnary | parseLiteral | parseFunctionCall | parseIdentifier, parseMultiplication | parseDivision | parseMod | parseEuclideanDivision | parseBooleanExpr), parseAddition | parseSubtraction), parseInequality | parseEquality)
  }

  private def parseBooleanExpr: BinOpParser = {
    parseAnd | parseOr | parseLesserEquals | parseLesser | parseGreaterEquals | parseGreater
  }

  /**
   * Useful method which associate left and right expression and operator to an BinaryOperation
   * @param op operator
   * @param left left expression
   * @param right right expression
   * @return a BinaryOperation composed of left, op, right
   */
  def binOp(op: Operator)(left: Expression, right: Expression): BinaryOperation = BinaryOperation(left, op, right)

  /**
   * Useful method which associate a token to an BinaryOperation
   * @param sepToken token which separate left and right expressions
   * @return a BinaryOperation parser of type Parser[(Expression, Expression) => BinaryOperation]
   */
  private def binOpParser(sepToken: Operator): BinOpParser = sepToken ^^^ binOp(sepToken)

  //Useful type used to parse BinaryOperation
  type BinOpParser = Parser[(Expression, Expression) => BinaryOperation]

  /*
  All parsers of BinaryOperations with implicit, example :
  def parseMod: BinOpParser = Mod ^^^ binOp(Mod) is equivalent to
  def parseMod: BinOpParser = Mod ^^^ { (left, right) => BinaryOperation(left, Mod, right) }
  because type of binOp is : binOP :: Token -> (Expression, Expression) -> BinaryOperation
   */
  private def parseMod: BinOpParser = binOpParser(Mod)
  private def parseDivision: BinOpParser = binOpParser(Slash)
  private def parseEuclideanDivision: BinOpParser = binOpParser(Percent)
  private def parseAddition: BinOpParser = binOpParser(Plus)
  private def parseSubtraction: BinOpParser = binOpParser(Minus)
  private def parseMultiplication: BinOpParser = binOpParser(Mul)
  private def parseEquality: BinOpParser = binOpParser(Equals)
  private def parseInequality: BinOpParser = binOpParser(NotEquals)
  private def parseAnd: BinOpParser = binOpParser(And)
  private def parseOr: BinOpParser = binOpParser(Or)
  private def parseLesser: BinOpParser = binOpParser(Less)
  private def parseLesserEquals: BinOpParser = binOpParser(LesserEqual)
  private def parseGreater: BinOpParser = binOpParser(Greater)
  private def parseGreaterEquals: BinOpParser = binOpParser(GreaterEqual)

  private def parseGrouping: Parser[Expression] = {
    if (devDebugMode) println("try to parse grouping")
    LeftParen ~> parseExpression <~ RightParen
  }

  private def parseExpression: Parser[Expression] = {
    if (devDebugMode) println("try to parse expression")
    parseGrouping | parseBinary | parseUnary | parseFunctionCall | parseLiteral
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
    (Function ~> parseIdentifier <~ LeftParen) ~ opt(parseTypeParameters) ~ (RightParen ~> opt(parseReturnType)) ^^ { case identifier ~ typeParameters ~ returnType =>
      val rType = returnType match {
        case Some(value) => value
        case None => UnitType
      }
      typeParameters match {
        case Some(value) => FunctionDeclaration(identifier, FunctionType(value, rType))
        case None => FunctionDeclaration(identifier, FunctionType(Seq.empty, rType))
      }
    }
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
      case None => TypeParameter(identifier, paramType, fr.emalios.algointerpreter.eval.In)
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

  def reader(tokens: Seq[Token]) = new TokenReader(tokens)

  /**
   * Method who try to generate an AST with tokens and if it does not succeed, it throws an error with the message provided by the lib.
   * @param tokens seq of tokens used to gen AST
   * @return an AST who represents a Program
   */
  def apply(tokens: Seq[Token]): Program = {
    this.parseProgram.apply(reader(tokens)) match {
      case NoSuccess(msg, _) => throw AlgoParsingError(msg)
      case Success(result, _) => result
    }
  }

  /**
   * Method who try to generate an AST with tokens and if it does not succeed, it throws an error with the message provided by the lib.
   * @param tokens seq of tokens used to gen AST
   * @return an AST who represents an expression
   */
  def applyInput(tokens: Seq[Token]): Expression = {
    this.parseExpression.apply(reader(tokens)) match {
      case NoSuccess(msg, _) => throw AlgoParsingError(msg)
      case Success(result, _) => result
    }
  }

}
