package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.eval.ASTEvaluator
import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser.{Affectation, BinaryOperation, BooleanLiteral, Expression, Identifier, Instruction, Literal, Number, StringLiteral, TokensParser, UnaryOperation}
import fr.emalios.algointerpreter.token.{And, Comma, Do, Dot, Else, End, EndFor, EndIf, EndWhile, Equals, False, For, From, Greater, GreaterEqual, If, LEFT_BOX_BRACKET, Lesser, LesserEqual, LeftParen, Mul, Minus, Mod, Not, NotEquals, Or, PERCENT, Plus, RIGHT_BOX_BRACKET, Return, RightParen, Slash, Start, StartLoop, Then, To, True, While}

import scala.collection.mutable


object Main extends AlgoLexer {

  val debugMode = true

  def main(args: Array[String]): Unit = {
    val lexer:AlgoLexer = new AlgoLexer
    val tokens = lexer.apply("Debut\n ecrire(\"Veuillez entrer une valeur quelconque\")\n x <- lire()\n ecrire(x)\nFin\n\nfonction afficher(nombre: entier)\nDebut\n ecrire(nombre)\nFin")
    println(tokens)
    val parser: TokensParser = new TokensParser
    val ast = parser.apply(tokens)
    var i: Int = 1
    val evaluator = new ASTEvaluator
    ast.mainAlgo.block.instructions.foreach(instruction => {
      //println("Press enter to display actual environment " + "instruction (" + i + ")")
      i+=1
      //scanner.nextLine()
      evaluator.addInstructionToEnv(instruction)
      if (debugMode) evaluator.printEnv()
    })
    if(debugMode) println(ast)
  }

}
