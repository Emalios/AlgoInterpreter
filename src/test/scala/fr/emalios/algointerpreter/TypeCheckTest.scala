package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.eval.AlgoTypeCheckingError
import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser._
import fr.emalios.algointerpreter.token.{And, Equals, Greater, GreaterEqual, Less, LesserEqual, Minus, Mod, Mul, Not, Or, Percent, Plus, Slash}
import fr.emalios.algointerpreter.typecheck.ASTTypeChecker
import fr.emalios.algointerpreter.typecheck.algow.{BooleanType, FunctionType, IntegerType, StringType}
import org.scalatest.FunSuite

class TypeCheckTest extends FunSuite {

  val typechecker = new ASTTypeChecker()

  test("integer expression does not pass typechecking") {
    val number = parser.Number(1)
    val inputs = List(
      BinaryOperation(number, Plus, number),
      BinaryOperation(number, Minus, number),
      BinaryOperation(number, Mod, number),
      BinaryOperation(number, Slash, number),
      BinaryOperation(number, Percent, number),
      BinaryOperation(number, Mul, number),
      UnaryOperation(Minus, number)
    )
    //inputs.foreach(operation => assert(this.typechecker.typeOf(operation) === IntegerType))
  }

  test("boolean expression does not pass typechecking") {
    val number = parser.Number(1)
    val string = parser.StringLiteral("str")
    val boolean = parser.BooleanLiteral(false)
    val inputs = List(
      BinaryOperation(number, Equals, number),
      BinaryOperation(string, Equals, string),
      BinaryOperation(boolean, Equals, boolean),
      BinaryOperation(number, Greater, number),
      BinaryOperation(number, GreaterEqual, number),
      BinaryOperation(number, Less, number),
      BinaryOperation(number, LesserEqual, number),
      BinaryOperation(boolean, And, boolean),
      BinaryOperation(boolean, Or, boolean),
      UnaryOperation(Not, boolean)
    )
    //inputs.foreach(operation => assert(this.typechecker.typeOf(operation) === BooleanType))
  }

  test("expr function call does not pass typechecking") {
    var ast = Program(Algo(Block(List(Assignment(Identifier("x"),FunctionCall(Identifier("f"),List(Number(1), StringLiteral("str")))), Assignment(Identifier("x"),BooleanLiteral(false))))),List(Function(FunctionDeclaration(Identifier("f"),FunctionType(List(TypeParameter(Identifier("x"),IntegerType, eval.In), TypeParameter(Identifier("s"),StringType,eval.In)),BooleanType)),Algo(Block(List(Return(BooleanLiteral(true))))))))
    //this.typechecker.typecheckAST(ast)
    ast = Program(Algo(Block(List(Assignment(Identifier("x"),FunctionCall(Identifier("f"),List(Number(1), StringLiteral("str")))), Assignment(Identifier("x"),Number(1))))),List(Function(FunctionDeclaration(Identifier("f"),FunctionType(List(TypeParameter(Identifier("x"),IntegerType, eval.In), TypeParameter(Identifier("s"),StringType,eval.In)),BooleanType)),Algo(Block(List(Return(BooleanLiteral(true))))))))
    intercept[AlgoTypeCheckingError] {
      //this.typechecker.typecheckAST(ast)
    }
  }

}
