package fr.emalios.algointerpreter.token

  sealed trait Token

  sealed trait Literal extends Token
  sealed trait Operator extends Token
  sealed trait UnaryOperator extends Operator

  case class Identifier(str: String) extends Token

  case object IntegerTypeToken extends Token {
    override def toString: String = "entier"
  }
  case object StringTypeToken extends Token {
    override def toString: String = "chaine"
  }
  case object BooleanTypeToken extends Token {
    override def toString: String = "booleen"
  }
  case object CharTypeToken extends Token {
    override def toString: String = "char"
  }
  case object RealTypeToken extends Token {
    override def toString: String = "reel"
  }

  case class IntegerToken(number: Int) extends Literal
  case class StringToken(value: String) extends Literal
  case class BooleanToken(value: Boolean) extends Literal

  case object InOut extends Token
  case object In extends Token
  case object Out extends Token

  case object Start extends Token {
    override def toString: String = "Debut" }
  case object End extends Token {
    override def toString: String = "Fin"
  }

  case object If extends Token {
    override def toString: String = "si"
  }
  case object EndIf extends Token {
    override def toString: String = "fsi"
  }
  case object Else extends Token {
    override def toString: String = "sinon"
  }

  case object From extends Token {
    override def toString: String = "de"
  }
  case object To extends Token {
    override def toString: String = "a"
  }
  case object Then extends Token {
    override def toString: String = "alors"
  }

  case object For extends Token {
    override def toString: String = "pour"
  }
  case object EndFor extends Token {
    override def toString: String = "fpour"
  }
  case object Do extends Token {
    override def toString: String = "faire"
  }

  case object While extends Token {
    override def toString: String = "tantque"
  }
  case object EndWhile extends Token {
    override def toString: String = "ftant"
  }

  case object Function extends Token {
    override def toString: String = "fonction"
  }
  case object Comma extends Token {
    override def toString: String = ","
  }
  case object Dot extends Token {
    override def toString: String = "."
  }

  case object RightParen extends Token {
    override def toString: String = ")"
  }
  case object LeftParen extends Token {
    override def toString: String = "("
  }

  case object EndOfLine extends Token

  case object RightBoxBracket extends Token {
    override def toString: String = "]"
  }
  case object LeftBoxBracket extends Token {
    override def toString: String = "["
  }

  case object Minus extends UnaryOperator {
    override def toString: String = "-"
  }
  case object Plus extends Operator {
    override def toString: String = "+"
  }
  case object GreaterEqual extends Operator {
    override def toString: String = ">="
  }

  case object LesserEqual extends Operator {
    override def toString: String = "<="
  }
  case object Less extends Operator {
    override def toString: String = "<"
  }
  case object Greater extends Operator {
    override def toString: String = ">"
  }
  case object Equals extends Operator {
    override def toString: String = "="
  }
  case object Slash extends Operator {
    override def toString: String = "/"
  }
  case object Percent extends Operator {
    override def toString: String = "%"
  }
  case object Mul extends Operator {
    override def toString: String = "*"
  }
  case object Or extends Operator {
    override def toString: String = "ou"
  }
  case object And extends Operator {
    override def toString: String = "et"
  }
  case object Mod extends Operator {
    override def toString: String = "mod"
  }
  case object NotEquals extends Operator {
    override def toString: String = "!="
  }
  case object Not extends UnaryOperator {
    override def toString: String = "!"
  }

  case object True extends Token
  case object False extends Token

  case object Return extends Token {
    override def toString: String = "retourne"
  }
  case object Affectation extends Token
  case object DoublePoints extends Token

