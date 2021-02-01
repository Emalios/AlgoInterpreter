package fr.emalios.algointerpreter.eval

import fr.emalios.algointerpreter.parser.{Block, FunctionDeclaration}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

abstract class Value {

  def unary_-(): Value = ???

  def unary_!(): Value = ???

  def ||(that: Value): Value = ???

  def &&(that: Value): Value = ???

  def !=(that: Value): Value = ???

  def *(that: Value): Value = ???

  def +(that: Value): Value = ???

  def -(that: Value): Value = ???

  def <(that: Value): Value = ???

  def <=(that: Value): Value = ???

  def >(that: Value): Value = ???

  def >=(that: Value): Value = ???

  def ==(that: Value): Value = ???

}

case class IntegerValue(value: AtomicInteger) extends Value {
  override def toString: String = this.value.toString

  override def +(that: Value): IntegerValue = {
    that match {
      case IntegerValue(value) => IntegerValue(new AtomicInteger(this.value.get() + value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def unary_-(): IntegerValue = IntegerValue(new AtomicInteger(-this.value.get()))

  override def -(that: Value): IntegerValue = {
    that match {
      case IntegerValue(value) => IntegerValue(new AtomicInteger(this.value.get() - value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def <(that: Value): BooleanValue = {
    that match {
      case IntegerValue(value) => BooleanValue(new AtomicBoolean(this.value.get() < value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def >(that: Value): BooleanValue = {
    that match {
      case IntegerValue(value) => BooleanValue(new AtomicBoolean(this.value.get() > value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def >=(that: Value): BooleanValue = {
    that match {
      case IntegerValue(value) => BooleanValue(new AtomicBoolean(this.value.get() >= value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def <=(that: Value): BooleanValue = {
    that match {
      case IntegerValue(value) => BooleanValue(new AtomicBoolean(this.value.get() <= value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def ==(that: Value): BooleanValue = {
    that match {
      case IntegerValue(value) => BooleanValue(new AtomicBoolean(this.value.get() == value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def !=(that: Value): BooleanValue = {
    that match {
      case IntegerValue(value) => BooleanValue(new AtomicBoolean(this.value.get() != value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def *(that: Value): IntegerValue = {
    that match {
      case IntegerValue(value) => IntegerValue(new AtomicInteger(this.value.get() * value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }
}

case class StringValue(value: String) extends Value {
  override def toString: String = this.value

}

case class BooleanValue(value: AtomicBoolean) extends Value {
  override def toString: String = this.value.toString

  override def &&(that: Value): BooleanValue = {
    that match {
      case BooleanValue(value) => BooleanValue(new AtomicBoolean(this.value.get() && value.get()))
    }
  }

  override def unary_!(): BooleanValue = {
    BooleanValue(new AtomicBoolean(!this.value.get()))
  }

  override def ||(that: Value): BooleanValue = {
    that match {
      case BooleanValue(value) => BooleanValue(new AtomicBoolean(this.value.get() || value.get()))
    }
  }

  override def !=(that: Value): BooleanValue = {
    that match {
      case IntegerValue(value) => BooleanValue(new AtomicBoolean(this.value.get() != value.get()))
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }
}

case class RealValue(value: Double) extends Value {
  override def toString: String = this.value.toString
}

case class CharValue(value: Char) extends Value {
  override def toString: String = this.value.toString
}

//TODO: replace by Option[Value] to avoid null return
case class PrimFunction(fun: Seq[Value] => Value) extends Value {
}

case class FunctionApplication(declaration: FunctionDeclaration, block: Block) extends Value {
}
