package fr.emalios.algointerpreter.typecheck.algow

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
  def showType(): String
}

case object IntegerType extends Type {
  override def showType(): String = "entier"
}
case object BooleanType extends Type {
  override def showType(): String = "booléen"
}
case object RealType extends Type {
  override def showType(): String = "réel"
}
case object StringType extends Type {
  override def showType(): String = "chaîne"
}
case object CharType extends Type {
  override def showType(): String = "charactère"
}
case class AnyType() extends Type {
  override def showType(): String = "Any"
}
case class TVar(name: String) extends Type {
  override def showType(): String = name

  override def equals(o: Any): Boolean = o match {
    case tVar: TVar => tVar.name.equals(this.name)
    case _ => false
  }
}

case object UnitType extends Type {
  override def showType(): String = "unit"
}

case object Undefined extends Type {
  override def showType(): String = "undefined"
}
case class FunctionType(parametersType: Seq[TypeParameter], returnType: Type) extends Type {
  override def showType(): String = s"function $parametersType $returnType"
}
