package fr.emalios.algointerpreter.token

  sealed trait Token

  sealed trait Literal extends Token
  sealed trait Operator extends Token
  sealed trait UnaryOperator extends Operator

  case class Identifier(str: String) extends Token

  case object IntegerTypeToken extends Token
  case object StringTypeToken extends Token
  case object BooleanTypeToken extends Token
  case object CharTypeToken extends Token
  case object RealTypeToken extends Token

  case class IntegerToken(number: Int) extends Literal
  case class StringToken(value: String) extends Literal
  case class BooleanToken(value: Boolean) extends Literal

  case object InOut extends Token
  case object In extends Token
  case object Out extends Token

  case object Start extends Token
  case object End extends Token

  case object If extends Token
  case object EndIf extends Token
  case object Else extends Token

  case object From extends Token
  case object To extends Token
  case object Then extends Token

  case object For extends Token
  case object EndFor extends Token
  case object Do extends Token

  case object While extends Token
  case object EndWhile extends Token

  case object Function extends Token
  case object Comma extends Token
  case object Dot extends Token

  case object RightParen extends Token
  case object LeftParen extends Token

  case object EndOfLine extends Token

  case object RightBoxBracket extends Token
  case object LeftBoxBracket extends Token

  case object Minus extends UnaryOperator
  case object Plus extends Operator
  case object GreaterEqual extends Operator

  case object LesserEqual extends Operator
  case object Less extends Operator
  case object Greater extends Operator
  case object Equals extends Operator
  case object Slash extends Operator
  case object Percent extends Operator
  case object Mul extends Operator
  case object Or extends Operator
  case object And extends Operator
  case object Mod extends Operator
  case object NotEquals extends Operator
  case object Not extends UnaryOperator

  case object True extends Token
  case object False extends Token

  case object Return extends Token
  case object Affectation extends Token
  case object DoublePoints extends Token

