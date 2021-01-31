package fr.emalios.algointerpreter.eval

import fr.emalios.algointerpreter.parser.{Block, FunctionDeclaration}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

class Value

  case class IntegerValue(value: AtomicInteger) extends Value {
    override def toString: String = this.value.toString
  }

  case class StringValue(value: String) extends Value {
    override def toString: String = this.value
  }

  case class BooleanValue(value: AtomicBoolean) extends Value {
    override def toString: String = this.value.toString
  }

  case class RealValue(value: Double) extends Value {
    override def toString: String = this.value.toString
  }

  case class CharValue(value: Char) extends Value {
    override def toString: String = this.value.toString
  }

  //TODO: replace by Option[Value] to avoid null return
  case class PrimFunction(fun: Seq[Value] => Value) extends Value

  case class FunctionApplication(declaration: FunctionDeclaration, block: Block) extends Value
