package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser._
import fr.emalios.algointerpreter.typecheck.WTypecheker
import fr.emalios.algointerpreter.typecheck.algow.{FunctionType, TVar, Type, Undefined}

import scala.io.Source

object Main extends AlgoLexer {

  val devDebugMode = false
  val debugMode = false

  def main(args: Array[String]): Unit = {

    val lexer:AlgoLexer = new AlgoLexer
    val filename = "input.txt"
    val source = Source.fromFile(filename)
    var code = ""
    for(line <- source.getLines())
      code = code ++ line ++ "\n"
    source.close()
    println(code)
    val tokens = lexer.apply(code)
    println(s"Tokens: $tokens")

    val parser: AlgoParser = new AlgoParser
    val ast = parser.apply(tokens)
    println(s"AST: $ast")

    val typechecker = new WTypecheker
    typechecker.typeInference(ast)
  }

}
