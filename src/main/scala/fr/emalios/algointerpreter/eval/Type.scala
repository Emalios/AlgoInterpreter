package fr.emalios.algointerpreter.eval

sealed trait Type

case object IntegerType extends Type
case object BooleanType extends Type
case object RealType extends Type
case object StringType extends Type
case object CharType extends Type
