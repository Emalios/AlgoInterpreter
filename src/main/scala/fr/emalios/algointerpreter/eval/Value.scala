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

  def showType(): String

}

case class IntegerValue(value: AtomicInteger) extends Value {
  override def toString: String = this.value.toString
  def this(value: Int) = {
    this(new AtomicInteger(value))
  }
  override def +(that: Value): Value = {
    that match {
      case IntegerValue(value) => new IntegerValue(this.value.get() + value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def unary_-(): Value = new IntegerValue(-this.value.get())

  override def -(that: Value): Value = {
    that match {
      case IntegerValue(value) => new IntegerValue(this.value.get() - value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def <(that: Value): Value = {
    that match {
      case IntegerValue(value) => new BooleanValue(this.value.get() < value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def >(that: Value): Value = {
    that match {
      case IntegerValue(value) => new BooleanValue(this.value.get() > value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def >=(that: Value): Value = {
    that match {
      case IntegerValue(value) => new BooleanValue(this.value.get() >= value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def <=(that: Value): Value = {
    that match {
      case IntegerValue(value) => new BooleanValue(this.value.get() <= value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def ==(that: Value): Value = {
    that match {
      case IntegerValue(value) => new BooleanValue(this.value.get() == value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def !=(that: Value): Value = {
    that match {
      case IntegerValue(value) => new BooleanValue(this.value.get() != value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def *(that: Value): Value = {
    that match {
      case IntegerValue(value) => new IntegerValue(this.value.get() * value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def showType(): String = "entier"
}

case class StringValue(value: String) extends Value {
  override def toString: String = this.value

  override def showType(): String = "chaîne"
}

case class BooleanValue(value: AtomicBoolean) extends Value {
  override def toString: String = this.value.toString
  def this(value: Boolean) = {
    this(new AtomicBoolean(value))
  }
  override def &&(that: Value): Value = {
    that match {
      case BooleanValue(value) => new BooleanValue(this.value.get() && value.get())
    }
  }

  override def unary_!(): Value = {
    new BooleanValue(!this.value.get())
  }

  override def ||(that: Value): Value = {
    that match {
      case BooleanValue(value) => new BooleanValue(this.value.get() || value.get())
    }
  }

  override def !=(that: Value): Value = {
    that match {
      case BooleanValue(value) => new BooleanValue(this.value.get() != value.get())
      case _ => /* TODO: make error messages */ throw AlgoEvaluationError("")
    }
  }

  override def showType(): String = "booléen"
}

case class RealValue(value: Double) extends Value {
  override def toString: String = this.value.toString

  override def showType(): String = "réel"
}

case class CharValue(value: Char) extends Value {
  override def toString: String = this.value.toString

  override def showType(): String = "charactère"
}

//TODO: replace by Option[Value] to avoid null return
case class PrimFunction(fun: Seq[Value] => Value) extends Value {
  override def showType(): String = "prim function"
}

case class FunctionApplication(declaration: FunctionDeclaration, block: Block) extends Value {
  override def showType(): String = "function"
}
