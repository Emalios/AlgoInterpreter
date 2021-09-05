package fr.emalios.algointerpreter.utils

import fr.emalios.algointerpreter.Main.Input
import fr.emalios.algointerpreter.typecheck.algow.Undefined

import scala.util.parsing.input.Reader

object ErrorUtil {


  def makeError(msg: String, reste: Reader[Any]): String = {
    var builder: StringBuilder = new StringBuilder
    //TODO: Remove hardcode for "input.txt"
    if (reste.pos.line == 0) return msg
    builder = builder.addAll(s"input.txt:${reste.pos.line}: $msg\n")
    val lines = reste.source.toString.split("\r?\n")
    val errorLine = lines(reste.pos.line-1)
    builder = builder.addAll(s"  ${reste.pos.line}  $errorLine\n")
    val pos = 4 + (reste.pos.column - 1) + reste.pos.line
    val truc: String = " " * (pos-1) + "^" + "\n"
    builder = builder.addAll(truc)
    builder.toString()
  }

}
