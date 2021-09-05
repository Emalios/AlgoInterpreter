package fr.emalios.algointerpreter.parser

import fr.emalios.algointerpreter.Main.Input
import fr.emalios.algointerpreter.parser.AlgoParser
import fr.emalios.algointerpreter.token.Token
import fr.emalios.algointerpreter.utils.ErrorUtil

import scala.util.parsing.combinator
import scala.util.parsing.input.Reader


case class AlgoParsingError(msg: String, reste: Reader[Token]) extends Exception() {

  override def toString: String = ErrorUtil.makeError(msg, reste)

}
