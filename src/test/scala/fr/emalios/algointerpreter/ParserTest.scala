package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser._
import fr.emalios.algointerpreter.token._
import fr.emalios.algointerpreter.typecheck.{BooleanType, FunctionType, IntegerType, StringType}
import org.scalatest.{BeforeAndAfter, FunSuite}

class ParserTest extends FunSuite with BeforeAndAfter {

  val tokensParser: AlgoParser = new AlgoParser()

  test("function call does not pass parsing") {
    var input = List(Start, EndOfLine, token.Identifier("f"), LeftParen, RightParen, EndOfLine, End)
    def output = this.tokensParser.apply(input)
    assert(output === Program(Algo(Block(List(ExprInstr(FunctionCall(parser.Identifier("f"), List()))))), List()))
    input = List(Start, EndOfLine, token.Identifier("f"), LeftParen, IntegerToken(8), Comma, StringToken("foo"), Comma, token.Identifier("g"), LeftParen, IntegerToken(1), RightParen, RightParen, EndOfLine, End)
    assert(output === Program(Algo(Block(List(ExprInstr(FunctionCall(parser.Identifier("f"), List(Number(8), StringLiteral("foo"), FunctionCall(parser.Identifier("g"), List(Number(1))))))))), List()))
  }

  test("affection does not pass parsing") {
    var input = List(Start, EndOfLine, token.Identifier("x"), Affectation, IntegerToken(1), EndOfLine, End)
    def output = this.tokensParser.apply(input)
    assert(output === Program(Algo(Block(List(parser.Assignment(parser.Identifier("x"), parser.Number(1))))), List()))
    input = List(Start, EndOfLine, token.Identifier("x"), Affectation, token.Identifier("f"), LeftParen, RightParen, EndOfLine, End)
    assert(output === Program(Algo(Block(List(Assignment(parser.Identifier("x"), FunctionCall(parser.Identifier("f"), List()))))), List()))
  }

  test("expressions do not pass parsing") {
    var input = List(Start, EndOfLine, token.Identifier("x"), Affectation, IntegerToken(1), Plus, LeftParen, IntegerToken(1), Minus, IntegerToken(1), RightParen, Mul, IntegerToken(1), Slash, IntegerToken(1), Percent, IntegerToken(1), EndOfLine, token.Identifier("y"), Affectation, BooleanToken(false), And, BooleanToken(true), Or, BooleanToken(false), EndOfLine, End)
    def output = this.tokensParser.apply(input)
    assert(output === Program(Algo(Block(List(Assignment(parser.Identifier("x"),BinaryOperation(Number(1),Plus,BinaryOperation(BinaryOperation(BinaryOperation(BinaryOperation(Number(1),Minus,Number(1)),Mul,Number(1)),Slash,Number(1)),Percent,Number(1)))), Assignment(parser.Identifier("y"),BinaryOperation(BinaryOperation(BooleanLiteral(false),And,BooleanLiteral(true)),Or,BooleanLiteral(false)))))),List()))
    input = List(Start, EndOfLine, token.Identifier("x"), Affectation, IntegerToken(1), Mod, IntegerToken(5), EndOfLine, token.Identifier("y"), Affectation, LeftParen, IntegerToken(5), LesserEqual, IntegerToken(5), GreaterEqual, IntegerToken(5), Greater, IntegerToken(5), Less, IntegerToken(5), Equals, BooleanToken(true), RightParen, EndOfLine, End)
    assert(output === Program(Algo(Block(List(Assignment(parser.Identifier("x"),BinaryOperation(Number(1),Mod,Number(5))), Assignment(parser.Identifier("y"),BinaryOperation(BinaryOperation(BinaryOperation(BinaryOperation(BinaryOperation(Number(5),LesserEqual,Number(5)),GreaterEqual,Number(5)),Greater,Number(5)),Less,Number(5)),Equals,BooleanLiteral(true)))))),List()))
  }

  test("if instruction does not pass parsing") {
    var input = List(Start, EndOfLine, If, token.Identifier("x"), Equals, IntegerToken(5), Then, EndOfLine, token.Identifier("x"), Affectation, IntegerToken(2), EndOfLine, EndIf, EndOfLine, End)
    def output = this.tokensParser.apply(input)
    assert(output === Program(Algo(Block(List(IfThenElseInstruction(BinaryOperation(parser.Identifier("x"),Equals,Number(5)),Block(List(Assignment(parser.Identifier("x"),Number(2)))),Some(Block(List())))))),List()))
    input = List(Start, EndOfLine, If, token.Identifier("x"), Equals, IntegerToken(5), Then, EndOfLine, token.Identifier("x"), Affectation, IntegerToken(2), EndOfLine, Else, EndOfLine, token.Identifier("x"), Affectation, IntegerToken(1), EndOfLine, EndIf, EndOfLine, End)
    assert(output === Program(Algo(Block(List(IfThenElseInstruction(BinaryOperation(parser.Identifier("x"),Equals,Number(5)),Block(List(Assignment(parser.Identifier("x"),Number(2)))),Some(Block(List(Assignment(parser.Identifier("x"),Number(1))))))))),List()))
    input = List(Start, EndOfLine, If, token.Identifier("x"), Equals, IntegerToken(5), Then, EndOfLine, token.Identifier("x"), Affectation, IntegerToken(2), EndOfLine, Else, If, token.Identifier("x"), Equals, IntegerToken(4), Then, EndOfLine, token.Identifier("x"), Affectation, IntegerToken(1), EndOfLine, EndIf, EndOfLine, EndIf, EndOfLine, End)
    assert(output === Program(Algo(Block(List(IfThenElseInstruction(BinaryOperation(parser.Identifier("x"),Equals,Number(5)),Block(List(Assignment(parser.Identifier("x"),Number(2)))),Some(Block(List(IfThenElseInstruction(BinaryOperation(parser.Identifier("x"),Equals,Number(4)),Block(List(Assignment(parser.Identifier("x"),Number(1)))),Some(Block(List())))))))))),List()))
  }

  test("for instruction does not pass parsing") {
    val input = List(Start, EndOfLine, For, token.Identifier("i"), From, IntegerToken(1), To, IntegerToken(10), Do, EndOfLine, token.Identifier("x"), Affectation, IntegerToken(1), EndOfLine, EndFor, EndOfLine, End)
    def output = this.tokensParser.apply(input)
    assert(output === Program(Algo(Block(List(ForInstruction(parser.Identifier("i"),Number(1),Number(10),Block(List(Assignment(parser.Identifier("x"),Number(1)))))))),List()))
  }

  test("while instruction does not pass parsing") {
    val input = List(Start, EndOfLine, token.Identifier("i"), Affectation, IntegerToken(1), EndOfLine, While, token.Identifier("i"), NotEquals, IntegerToken(0), Do, EndOfLine, token.Identifier("i"), Affectation, token.Identifier("i"), Minus, IntegerToken(1), EndOfLine, EndWhile, EndOfLine, End)
    def output = this.tokensParser.apply(input)
    assert(output === Program(Algo(Block(List(Assignment(parser.Identifier("i"),Number(1)), WhileInstruction(BinaryOperation(parser.Identifier("i"),NotEquals,Number(0)),Block(List(Assignment(parser.Identifier("i"),BinaryOperation(parser.Identifier("i"),Minus,Number(1))))))))),List()))
  }

  //TODO: invest about why it dont pass
  test("function declaration does not pass parsing") {
    val input = List(Start, EndOfLine, token.Identifier("f"), LeftParen, IntegerToken(1), Comma, StringToken("truc"), RightParen, EndOfLine, End, EndOfLine, token.Function, token.Identifier("f"), LeftParen, token.Identifier("x"), DoublePoints, IntegerTypeToken, Comma, token.Identifier("s"), DoublePoints, StringTypeToken, RightParen, DoublePoints, BooleanTypeToken, EndOfLine, Start, EndOfLine, token.Return, BooleanToken(true), EndOfLine, End)
    def output = this.tokensParser.apply(input)
    assert(output === Program(Algo(Block(List(ExprInstr(FunctionCall(parser.Identifier("f"),List(Number(1), StringLiteral("truc"))))))),List(parser.Function(FunctionDeclaration(parser.Identifier("f"),FunctionType(Some(List(TypeParameter(parser.Identifier("x"),IntegerType, eval.In), TypeParameter(parser.Identifier("s"),StringType, eval.In))),Some(BooleanType))),Algo(Block(List(parser.Return(BooleanLiteral(true)))))))))
  }

}
