package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.eval.AlgoEvaluator
import fr.emalios.algointerpreter.lexer.{AlgoLexer, AlgoLexerError}
import fr.emalios.algointerpreter.parser._
import fr.emalios.algointerpreter.token.{Minus, Plus}
import fr.emalios.algointerpreter.typecheck.WTypecheker
import fr.emalios.algointerpreter.typecheck.algow.{FunctionType, IntegerType, TVar, Type, Undefined}

import scala.io.Source

object Main extends AlgoLexer {

  val devDebugMode = false
  val debugMode = false

  def main(args: Array[String]): Unit = {
    val lexer: AlgoLexer = new AlgoLexer()
    /* Get code from input source */
    val filename = "input.txt"
    val source = Source.fromFile(filename)
    var code = ""
    for(line <- source.getLines())
      code = code ++ line ++ "\n"
    source.close()
    println(code)

    /* token generation */
    try {
      val tokens = lexer.apply(code)
      //println(s"Tokens: $tokens")

      /* ast generation */
      val parser: AlgoParser = new AlgoParser()
      val ast = parser.apply(tokens)
      //println(s"AST: $ast")
      /* typecheck */
      val typechecker = new WTypecheker()
      typechecker.typeInference(ast)
      /* runtime */
      val evaluator = new AlgoEvaluator()
      evaluator.evalProgram(ast)
    } catch {
      case e: AlgoLexerError => System.err.println(e)
    }

  }

}
