package fr.emalios.algointerpreter.parser

class Value

  case class IntegerValue(value: Int) extends Value {
    override def toString: String = this.value.toString
  }
  case class StringValue(value: String) extends Value {
    override def toString: String = this.value
  }
  case class BooleanValue(value: String) extends Value {
    override def toString: String = this.value
  }

