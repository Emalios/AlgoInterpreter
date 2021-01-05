package fr.emalios.algointerpreter.token

  sealed trait Token

  sealed trait Literal extends Token

  case class Identifier(str: String) extends Token

  case class IntegerToken(number: Int) extends Literal
  case object IntegerType extends Token
  case object StringType extends Token
  case object CharType extends Token
  case object RealType extends Token

  case class StringToken(value: String) extends Literal

  case class BooleanToken(value: Boolean) extends Literal

  case object Start extends Token
  case object End extends Token

  case object If extends Token

  case object EndIf extends Token

  case object For extends Token

  case object EndFor extends Token

  case object While extends Token

  case object EndWhile extends Token

  case object Function extends Token

  case object Comma extends Token

  case object Dot extends Token

  case object RightParen extends Token

  case object EndOfLine extends Token

  case object LeftParen extends Token

  case object RIGHT_BOX_BRACKET extends Token

  case object LEFT_BOX_BRACKET extends Token

  case object Minus extends Token

  case object Plus extends Token

  case object Affectation extends Token

  case object GreaterEqual extends Token

  case object LesserEqual extends Token

  case object Lesser extends Token

  case object Greater extends Token

  case object Equals extends Token

  case object Slash extends Token

  case object PERCENT extends Token

  case object Mul extends Token

  case object True extends Token

  case object False extends Token

  case object Return extends Token

  case object And extends Token

  case object Or extends Token

  case object StartLoop extends Token

  case object From extends Token

  case object To extends Token

  case object Then extends Token

  case object Else extends Token

  case object Not extends Token

  case object NotEquals extends Token

  case object Mod extends Token
  case object Do extends Token

