package fr.emalios.algointerpreter.lexer
import fr.emalios.algointerpreter.Main.{Elem, Input}
import fr.emalios.algointerpreter.utils.ErrorUtil

import java.io.{PrintStream, PrintWriter}
import scala.util.parsing.input.Position

case class AlgoLexerError(msg: String, reste: Input) extends Exception {

  override def toString: String = ErrorUtil.makeError(msg, reste)

}
