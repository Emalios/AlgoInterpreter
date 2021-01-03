package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.eval.ASTEvaluator
import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser.{Affectation, BinaryOperation, BooleanLiteral, BooleanValue, Expression, Identifier, Instruction, IntegerValue, Literal, Number, StringLiteral, StringValue, TokensParser, UnaryOperation, Value}
import fr.emalios.algointerpreter.token.{AND, Comma, Do, Dot, Else, End, EndFor, EndIf, EndWhile, Equals, False, For, From, GREATER, GREATER_EQUAL, If, LEFT_BOX_BRACKET, LESSER, LESSER_EQUAL, LeftParen, Mul, Minus, Mod, Not, NotEquals, OR, PERCENT, Plus, RIGHT_BOX_BRACKET, ReadInput, Return, RightParen, Slash, Start, StartLoop, Then, To, True, While}

import scala.collection.mutable


object Main extends AlgoLexer {

  def main(args: Array[String]): Unit = {
    val lexer:AlgoLexer = new AlgoLexer
    val tokens = lexer.apply("Debut \n x <- 5 \n Fin")
    println(tokens)
    val parser: TokensParser = new TokensParser
    val ast = parser.apply(tokens)
    val scanner = new java.util.Scanner(System.in)
    var i: Int = 1
    val evaluator = new ASTEvaluator
    ast.block.instructions.foreach(instruction => {
      println("Press enter to display actual environment " + "instruction (" + i + ")")
      i+=1
      scanner.nextLine()
      evaluator.addInstructionToEnv(instruction)
      evaluator.printEnv()
    })
    println(ast)
  }

}
