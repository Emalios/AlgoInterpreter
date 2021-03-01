package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.eval.{AlgoEvaluator, In}
import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser._
import fr.emalios.algointerpreter.token.{Mod, Not, Plus}
import fr.emalios.algointerpreter.typecheck.{ASTTypeChecker, FunctionType, IntegerType, TVar, WTypechecker}

object Main extends AlgoLexer {

  val devDebugMode = false
  val debugMode = false

  def main(args: Array[String]): Unit = {

    /*
    val lexer:AlgoLexer = new AlgoLexer
    val tokens = lexer.apply("Debut\nt <- lire()\n ecrire(x)\n x <- 5+6*vrai\n y <- 5 = x\n pour x de 1 a t faire\n ecrire(1)\n fpour\nFin")
    if (debugMode) println(tokens)

    val parser: AlgoParser = new AlgoParser
    val ast = parser.apply(tokens)
    if (debugMode) println(ast)

    val typechecker = new ASTTypeChecker
    typechecker.typecheckAST(ast)
    typechecker.print()


    val evaluator = new AlgoEvaluator
    evaluator.evalProgram(ast)

     */

    val typeChecker = new WTypechecker
    val truc = FunctionType(Option(Seq(TypeParameter(Identifier("first"), TVar("a"), In), TypeParameter(Identifier("second"), TVar("b"), In), TypeParameter(Identifier("last"), IntegerType, In))), Option(TVar("c")))
    println(typeChecker.ftv(truc))

  }

}
