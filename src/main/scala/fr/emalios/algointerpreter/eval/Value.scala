package fr.emalios.algointerpreter.eval

import fr.emalios.algointerpreter.parser.Block

class Value

  case class IntegerValue(value: Int) extends Value {
    override def toString: String = this.value.toString
  }
  case class StringValue(value: String) extends Value {
    override def toString: String = this.value
  }
  case class BooleanValue(value: Boolean) extends Value {
    override def toString: String = this.value.toString
  }

case class FunctionValue(args: List[(String, Value)], body: Block) extends Value

