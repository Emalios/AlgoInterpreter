package fr.emalios.algointerpreter.typecheck

import fr.emalios.algointerpreter.parser.Identifier

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class WTypechecker {

  type Substitution = mutable.Map[String, Type]
  type Frame = mutable.HashMap[Identifier, Type]
  type Stack = mutable.ArrayBuffer[Frame]
  private var stack: Stack = mutable.ArrayBuffer[Frame]()
  private val globalFrame: Frame = mutable.HashMap[Identifier, Type]()

  def ftv(typeOf: Type): List[TVar] = {
    typeOf match {
      case IntegerType => List.empty
      case BooleanType => List.empty
      case RealType => List.empty
      case StringType => List.empty
      case CharType => List.empty
      case AnyType() => List.empty
      case tVar: TVar => List(tVar)
      case Undefined => List.empty
      case FunctionType(parametersType, returnType) =>
        val paramFtv: ListBuffer[TVar] = ListBuffer.empty
        parametersType match {
          case Some(value) => value.foreach(param => paramFtv.addAll(this.ftv(param.paramType)))
          case None =>
        }
        val returnFtv : ListBuffer[TVar] = ListBuffer.empty
        returnType match {
          case Some(value) => returnFtv.addAll(this.ftv(value))
          case None =>
        }
        (paramFtv ++ returnFtv).toList
    }
  }

  def apply(substitution: Substitution, typeOf: Type): Type = {
    typeOf match {
      case IntegerType => typeOf
      case BooleanType => typeOf
      case RealType => typeOf
      case StringType => typeOf
      case CharType => typeOf
      case AnyType() => typeOf
      case tVar: TVar => substitution.get(tVar.name) match {
        case Some(value) => value
        case None => tVar
      }
      case Undefined => typeOf
      case FunctionType(parametersType, returnType) =>
        val paramSubs: Seq[Type] = Seq.empty
        parametersType match {
          case Some(value) => value.foreach(param => paramSubs = paramSubs.()this.apply(substitution, param.paramType))
          case None =>
        }
        null
    }
  }

}
