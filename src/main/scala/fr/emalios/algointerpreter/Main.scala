package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser.TokensParser
import fr.emalios.algointerpreter.token.Token

object Main extends AlgoLexer {

  def main(args: Array[String]): Unit = {
    val lexer:AlgoLexer = new AlgoLexer
    val tokens = lexer.apply("Debut\n    somme <- 0\n    Pour i de 1 a 10 faire\n        valeur <- lire()\n        somme <- somme + valeur\n    fpour\n    ecrire(somme)\nFin")
    println(tokens)
  }

}
