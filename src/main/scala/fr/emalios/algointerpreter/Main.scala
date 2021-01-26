package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.eval.ASTEvaluator
import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser._


object Main extends AlgoLexer {

  val debugMode = false

  def main(args: Array[String]): Unit = {
    val lexer:AlgoLexer = new AlgoLexer
    val tokens = lexer.apply("Debut\n ecrire(\"Entrez une valeur\") \nx <- lire()\n ecrire(x*x)\n Fin \n\n\n fonction puissance2(nombre: entier)\nDebut\n    x <- nombre * nombre\n    ecrire(x)\nFin")
    if (debugMode) println(tokens)
    val parser: TokensParser = new TokensParser
    val ast = parser.apply(tokens)
    if (debugMode) println("ast functions : " + ast.declaredFunction)
    if(debugMode) println(ast)
    val evaluator = new ASTEvaluator
    evaluator.evalProgram(ast)
  }

}
