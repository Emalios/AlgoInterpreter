package fr.emalios.algointerpreter.typecheck

import fr.emalios.algointerpreter.parser.TypeParameter

sealed trait Type {
  override def equals(o: Any): Boolean = {
    o match {
      case t: Type =>
        t match {
        case _: AnyType => true
        case _ => super.equals(o)
      }
      case _ => false
    }
  }
}

case object IntegerType extends Type {
}
case object BooleanType extends Type
case object RealType extends Type
case object StringType extends Type
case object CharType extends Type
case class AnyType() extends Type
case class FunctionType(parametersType: Option[Seq[TypeParameter]], returnType: Option[Type]) extends Type
