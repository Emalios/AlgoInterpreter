package fr.emalios.algointerpreter.eval

import fr.emalios.algointerpreter.Main.devDebugMode
import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser._
import fr.emalios.algointerpreter.parser.{Assignment, BinaryOperation, Block, BooleanLiteral, ExprInstr, Expression, FunctionCall, Identifier, Instruction, Literal, Number, StringLiteral, UnaryOperation}
import fr.emalios.algointerpreter.token.{And, Equals, Greater, GreaterEqual, Lesser, LesserEqual, Minus, Mul, Not, NotEquals, Or, Plus}

import scala.collection.mutable

class ASTEvaluator() {

  //Type used at runtime
  type Frame = mutable.HashMap[Identifier, (Value, Quantifier)]
  type CallStack = mutable.ArrayBuffer[Frame]
  private var callStack: CallStack = mutable.ArrayBuffer[Frame]()

  //builtin functions
  private val defaultFunctionsFrame: Frame = new mutable.HashMap[Identifier, (Value, Quantifier)]()
  this.defaultFunctionsFrame.addOne((Identifier("ecrire"), (PrimFunction(writeValue), In)))
  this.defaultFunctionsFrame.addOne((Identifier("lire"), (PrimFunction(_ => readValue()), In)))
  this.callStack += defaultFunctionsFrame

  //builtin function for `lire()`
  private def readValue(): Value = {
    val scanner = new java.util.Scanner(System.in)
    val tokens = new AlgoLexer().apply(scanner.nextLine())
    val expression = new TokensParser().applyInput(tokens)
    evalExpression(expression)._1.get
  }

  //builtin function for `ecrire(Valeur...)`
  private def writeValue(params: Seq[Value]) = {
    params.foreach(println)
    null
  }

  def printEnv(): Unit = {
    printDelimiter(16)
    this.getCurrentFrame.foreachEntry((identifier, expression) => {
      println(identifier.value + "|" + expression)
    })
    printDelimiter(16)
  }

  private def printDelimiter(length: Int): Unit = {
    for (_ <- 1 to length)
      print("-")
    println()
  }

  private def getCurrentFrame: Frame = this.callStack.last

  def evalProgram(program: Program): Unit = {
    //Add all functions to global scopes
    if (devDebugMode) println("add functions")
    program.declaredFunction.foreach(this.addFunctionToGlobalScope)
    if (devDebugMode) println(this.getCurrentFrame)
    this.evalBlock(program.mainAlgo.block)
  }

  def addFunctionToGlobalScope(function: Function): Unit = {
    if (devDebugMode) println("Ajout de la fonction: " + function.declaration.functionName + " au scope global.")
    this.callStack.head.addOne((function.declaration.functionName, (FunctionApplication(function.declaration, function.algo.block), In)))
  }

  def evalInstruction(instruction: Instruction): (Option[Value], Boolean) =
    instruction match {
      case Assignment(identifier, expression) =>
        val expressionValue = this.evalExpression(expression)._1.get
        if (this.getCurrentFrame.contains(identifier)) this.getCurrentFrame(identifier)._2 match {
          case In => throw AlgoEvaluationError("Erreur: Vous ne pouvez pas modifier la variable '" + identifier.value + "'")
          case _ =>
            val quantifier = this.getCurrentFrame(identifier)._2
            this.getCurrentFrame.update(identifier, (expressionValue, quantifier))
        } else this.getCurrentFrame.addOne(identifier, (expressionValue, InOut))
        (Option.empty, false)
      case exprInstr: ExprInstr => this.evalExpression(exprInstr.e)
      case ForInstruction(identifier, expressionFrom, expressionTo, block) =>
        val valueFrom = this.evalExpression(expressionFrom)._1.get match {
          case v: IntegerValue => v
        }
        val valueTo = this.evalExpression(expressionTo)._1.get match {
          case v: IntegerValue => v
        }
        val from = valueFrom.value.get()
        val to = valueTo.value.get()
        //from must be < to
        if (from > to) throw AlgoEvaluationError("Erreur: " + from + " < " + to + "")
        val frame: Frame = this.getCurrentFrame.clone()
        val integerValue = new IntegerValue(valueFrom.value.get())
        //shadow variable if already present
        if (this.getCurrentFrame.contains(identifier)) frame.update(identifier, (integerValue, In)) else frame.addOne(identifier, (integerValue, In))
        //update callstack
        this.callStack.addOne(frame)
        //eval for instruction
        for (i <- valueFrom.value.get() to valueTo.value.get()) {
          //update variable
          integerValue.value.set(i)
          this.evalBlock(block)
        }
        //pop
        this.callStack.remove(this.callStack.length - 1)
        (Option.empty, false)
      case WhileInstruction(condition, block) =>
        def evaluatedCondition = this.evalExpression(condition)._1.get match {
          case v: BooleanValue => v
        }
        //update callstack (push)
        val frame: Frame = this.getCurrentFrame.clone()
        this.callStack.addOne(frame)
        //eval while instruction
        while (evaluatedCondition.value.get()) {
          this.evalBlock(block)
        }
        //pop
        this.callStack.remove(this.callStack.length-1)
        (Option.empty, false)
      case IfThenElseInstruction(condition, thenBlock, elseBlock) =>
        val conditionValue = this.evalExpression(condition)._1.get match {
          case v: BooleanValue => v
        }
        val frame: Frame = this.getCurrentFrame.clone()
        var blockValue: (Option[Value], Boolean) = (Option.empty, false)
        this.callStack.addOne(frame)
        if (conditionValue.value.get()) {
          blockValue = this.evalBlock(thenBlock)
        } else {
          elseBlock match {
            case Some(value) => blockValue = this.evalBlock(value)
            case None =>
          }
        }
        this.callStack.remove(this.callStack.length-1)
        blockValue
      case Return(expression) => (this.evalExpression(expression)._1, true)
    }

  private def callFunction(functionName: Identifier, values: List[Expression]): (Option[Value], Boolean) = {
    if (this.getCurrentFrame.contains(functionName)) {
      this.getCurrentFrame(functionName) match {
        case (PrimFunction(function), _) => (Option(function.apply(values.map(this.evalExpression).map(optionalValue => optionalValue._1.get))), false)
        case (FunctionApplication(declaration, block), _) =>
          val frame: Frame = this.callStack.head.clone()
          for ((typeParameter, value) <- declaration.functionType.parametersType.get zip values) {
            val expressionValue: Value = this.evalExpression(value)._1.get
            frame.addOne((typeParameter.name, (expressionValue, typeParameter.quantifier)))
          }
          this.callStack.addOne(frame)
          val value = this.evalBlock(block)
          this.callStack.remove(this.callStack.length-1)
          value
      }
    }
    else throw AlgoEvaluationError("Erreur: La fonction '" + functionName + "' n'existe pas.")
  }

  def evalBlock(block: Block): (Option[Value], Boolean) = {
    for (instruction <- block.instructions) {
      val result = this.evalInstruction(instruction)
      if (result._2) return result
    }
    (Option.empty, false)
  }

  def checkValueNotEmpty(x: Option[Value], err: String): Value = x match {
    case Some(value)  => value
    case None         => throw AlgoEvaluationError(err)
  }

  private def evalExpression(expression: Expression): (Option[Value], Boolean) = {
    expression match {
      case FunctionCall(functionName, args) => this.callFunction(functionName, args)
      case UnaryOperation(operator, rightExpression) =>
        val (rightResult, _) = this.evalExpression(rightExpression)
        val right = this.checkValueNotEmpty(rightResult, "Trying to unwrap no value")
        operator match {
          case Minus  => (Some(right.unary_-()), false)
          case Not    => (Some(right.unary_!()), false)
      }
      case BinaryOperation(leftExpression, operator, rightExpression) =>
        val (leftResult, _) = this.evalExpression(leftExpression)
        val (rightResult, _) = this.evalExpression(rightExpression)
        val left = checkValueNotEmpty(leftResult, "Trying to unwrap no value")
        val right = checkValueNotEmpty(rightResult, "Trying to unwrap no value")

        operator match {
          case Plus         => (Some(left + right), false)
          case Minus        => (Some(left - right), false)
          case Mul          => (Some(left * right), false)
          case Lesser       => (Some(left < right), false)
          case LesserEqual  => (Some(left <= right), false)
          case Greater      => (Some(left > right), false)
          case GreaterEqual => (Some(left >= right), false)
          case Equals       => (Some(left == right), false)
          case NotEquals    => (Some(left != right), false)
          case And          => (Some(left && right), false)
          case Or           => (Some(left || right), false)
        }
      case identifier: Identifier =>
        if (this.getCurrentFrame.contains(identifier)) this.getCurrentFrame(identifier)._2 match {
          case Out => throw AlgoEvaluationError("Erreur: Vous ne pouvez pas lire cette variable : '" + identifier.value + "'")
          case _ => this.getCurrentFrame(identifier)._1 match {
            case value: IntegerValue  => (Option(value), false)
            case value: RealValue     => (Option(value), false)
            case value: BooleanValue  => (Option(value), false)
            case value: StringValue   => (Option(value), false)
            case value: CharValue     => (Option(value), false)
          }
        }
        else throw AlgoEvaluationError("Erreur: L'identifieur: '" + identifier.value + "' n'existe pas dans la portée actuelle.")
      case literal: Literal => literal match {
        case StringLiteral(value)   => (Option(StringValue(value)), false)
        case Number(value)          => (Option(new IntegerValue(value)), false)
        case BooleanLiteral(value)  => (Option(new BooleanValue(value)), false)
      }
    }
  }

}
