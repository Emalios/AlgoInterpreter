package fr.emalios.algointerpreter.lexer
import fr.emalios.algointerpreter.Main.{Elem, Input}

import java.io.{PrintStream, PrintWriter}
import scala.util.parsing.input.Position

case class AlgoLexerError(msg: String, reste: Input) extends Exception {

  override def toString: String = {
    var s = s"input.txt:${reste.pos.line}: $msg\n"
    val lines = reste.source.toString.split("\r?\n")
    val errorLine = lines(reste.pos.line-1)
    s += s"  ${reste.pos.line}  $errorLine\n"
    val pos = 4 + (reste.pos.column - 1) + reste.pos.line
    s += " ".repeat(pos-1) + "^" + "\n"
    s
  }

}
