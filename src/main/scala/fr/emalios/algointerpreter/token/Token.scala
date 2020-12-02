package fr.emalios.algointerpreter.token

  sealed trait Token
  sealed trait Operator extends Token

  case class Identifier(str: String) extends Token

  case class Literal(str: String) extends Token

  case class Number(number: Int) extends Token

  case object Start extends Token

  case object End extends Token

  case object If extends Token

  case object EndIf extends Token

  case object For extends Token

  case object EndFor extends Token

  case object While extends Token

  case object EndWhile extends Token

  case object Function extends Token

  case object ReadInput extends Token

  case object Comma extends Token

  case object Dot extends Token

  case object RightParen extends Operator

  case object LeftParen extends Operator

  case object RIGHT_BOX_BRACKET extends Token

  case object LEFT_BOX_BRACKET extends Token

  case object Minus extends Operator

  case object PLUS extends Operator

  case object AFFECTATION extends Operator

  case object GREATER_EQUAL extends Operator

  case object LESSER_EQUAL extends Operator

  case object LESSER extends Operator

  case object GREATER extends Operator

  case object EQUALS extends Operator

  case object SLASH extends Operator

  case object PERCENT extends Operator

  case object MUL extends Operator

  case object TRUE extends Token

  case object FALSE extends Token

  case object RETURN extends Token

  case object AND extends Token

  case object OR extends Token

  case object StartLoop extends Token

  case object From extends Token

  case object To extends Token

  case object Then extends Token

  case object Else extends Token

  case object Not extends Operator

  case object NotEquals extends Operator

  case object Mod extends Operator
  case object Do extends Token

