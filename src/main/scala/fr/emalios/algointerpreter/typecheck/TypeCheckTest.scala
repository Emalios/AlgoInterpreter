package fr.emalios.algointerpreter.typecheck

import fr.emalios.algointerpreter.eval.In
import fr.emalios.algointerpreter.parser.{Identifier, TypeParameter}
import fr.emalios.algointerpreter.typecheck.algow.{AnyType, FunctionType, IntegerType, TVar, Type, UnitType}

import scala.collection.mutable

object TypeCheckTest {

  def main(args: Array[String]): Unit = {
    val typecheck = new WTypecheker
    //type Scheme = (List[String], Type)
    //type Subst = Map[String, Type]
    //type TypeEnv = mutable.Map[String, Scheme]
    val typeEnv: mutable.Map[String, (List[String], Type)] = mutable.Map("i" -> (List("a"), TVar("a")))
    val map = Map[String, Type](("t0" -> algow.IntegerType))
    println(typecheck.instantiate((List("t"), TVar("t"))))
  }

}
