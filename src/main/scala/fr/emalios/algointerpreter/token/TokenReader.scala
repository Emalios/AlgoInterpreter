package fr.emalios.algointerpreter.token

import scala.util.parsing.input.{NoPosition, Position, Reader}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {

  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)

}
