package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.eval.ASTEvaluator
import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser._

import java.util.concurrent.atomic.AtomicInteger


object Main extends AlgoLexer {

  val devDebugMode = false
  val debugMode = true

  def main(args: Array[String]): Unit = {
    val lexer:AlgoLexer = new AlgoLexer
    val tokens = lexer.apply("Debut\n    i <- 1\n    tantque i != 10 faire\n        ecrire(i)\n        i <- i + 1\n    ftant\n    ecrire(i)\nFin")
    if (debugMode) println(tokens)
    val parser: TokensParser = new TokensParser
    val ast = parser.apply(tokens)
    if (debugMode) println("available functions : " + ast.declaredFunction)
    if(debugMode) println(ast)
    val evaluator = new ASTEvaluator
    evaluator.evalProgram(ast)
  }

/*
  def main(args: Array[String]):Unit = {
    var x: AtomicInteger = new AtomicInteger(5)
    var y = x
    y.set(1)
    println(x)
  }
 */

}
