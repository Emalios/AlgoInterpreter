package fr.emalios.algointerpreter.eval

import com.ibm.jvm.Trace
import fr.emalios.algointerpreter.Main.debugMode
import fr.emalios.algointerpreter.lexer.{AlgoLexer, AlgoLexerError}
import fr.emalios.algointerpreter.parser._
import fr.emalios.algointerpreter.eval._
import fr.emalios.algointerpreter.parser.{Affectation, BinaryOperation, Block, BooleanLiteral, ExprInstr, Expression, FunctionCall, Identifier, Instruction, Literal, Number, StringLiteral, UnaryOperation}
import fr.emalios.algointerpreter.token.{And, Equals, Greater, GreaterEqual, Lesser, LesserEqual, Minus, Mul, Not, Or, Plus}

import java.util.Scanner
import scala.collection.mutable

class ASTEvaluator() {

  //Type used at runtime
  type Frame = mutable.HashMap[Identifier, Value]
  type CallStack = mutable.ArrayBuffer[Frame]
  private var callStack: CallStack = mutable.ArrayBuffer[Frame]()

  //builtin functions
  private val defaultFunctionsFrame: Frame = new mutable.HashMap[Identifier, Value]()
  this.defaultFunctionsFrame.addOne((Identifier("ecrire"), PrimFunction(writeValue)))
  this.defaultFunctionsFrame.addOne((Identifier("lire"), PrimFunction(_ => readValue())))
  this.callStack += defaultFunctionsFrame

  //builtin function for `lire()`
  private def readValue(): Value = {
    val scanner = new java.util.Scanner(System.in)
    val tokens = new AlgoLexer().apply(scanner.nextLine())
    val expression = new TokensParser().applyInput(tokens)
    evalExpression(expression)
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
    for (_ <- 1 to length) print("-")
    println()
  }

  private def getCurrentFrame: Frame = this.callStack.last

  def evalProgram(program: Program): Unit = {
    //Add all functions to global scopes
    if (debugMode) println("add functions")
    program.declaredFunction.foreach(this.addFunctionToGlobalScope)
    if (debugMode) println(this.getCurrentFrame)
    this.evalBlock(program.mainAlgo.block)
  }

  def addFunctionToGlobalScope(function: Function): Unit = {
    if (debugMode) println("Ajout de la fonction: " + function.declaration.functionName + " au scope global.")
    this.callStack.head.addOne((function.declaration.functionName, FunctionApplication(function.declaration, function.algo.block)))
  }

  def evalInstruction(instruction: Instruction): Unit = {
    instruction match {
      case Affectation(identifier, expression) => this.getCurrentFrame.update(identifier, this.evalExpression(expression))
      case exprInstr: ExprInstr => exprInstr.e match {
        case FunctionCall(functionName, args) => functionName match {
          case _ =>  callFunction(functionName, args)
        }
      }
    }
  }

  private def callFunction(functionName: Identifier, values: List[Expression]): Value = {
    if (this.getCurrentFrame.contains(functionName)) {
      this.getCurrentFrame(functionName) match {
        case PrimFunction(function) => function.apply(values.map(evalExpression))
      }
    }
    else throw AlgoEvaluationError("Erreur: La fonction '" + functionName + "' n'existe pas.")
  }

  def evalBlock(block: Block): Unit = {
    block.instructions.foreach(this.evalInstruction)
  }

  private def evalExpression(expression: Expression): Value = {
    expression match {
      case FunctionCall(functionName, args) => this.callFunction(functionName, args)
      case UnaryOperation(operator, right) => operator match {
        case Minus => IntegerValue(evalExpression(right) match {
          case IntegerValue(value) => -value
        })
        case Not => BooleanValue(evalExpression(right) match {
            case BooleanValue(value) => value
        })
      }
      case BinaryOperation(left, operator, right) => operator match {
        case Plus => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => IntegerValue(left.value + right.value)
          }
        }
        case Minus => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => IntegerValue(left.value - right.value)
          }
        }
        case Mul => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => IntegerValue(left.value * right.value)
          }
        }
        case Lesser => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(left.value < right.value)
          }
        }
        case LesserEqual => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(left.value <= right.value)
          }
        }
        case Greater => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(left.value > right.value)
          }
        }
        case GreaterEqual => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(left.value >= right.value)
          }
        }
        case Equals => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(left.value == right.value)
          }
        }
        case And => evalExpression(left) match {
          case left: BooleanValue => evalExpression(right) match {
            case right: BooleanValue => BooleanValue(left.value && right.value)
          }
        }
        case Or => evalExpression(left) match {
          case left: BooleanValue => evalExpression(right) match {
            case right: BooleanValue => BooleanValue(left.value || right.value)
          }
        }
      }
      case identifier: Identifier =>
        if (this.getCurrentFrame.contains(identifier)) this.getCurrentFrame.get(identifier) match {
          case Some(value) => value match {
            case literal => literal
            case IntegerValue(value) => IntegerValue(value)
            case BooleanValue(value) => BooleanValue(value)
          }
        } else throw AlgoEvaluationError("Erreur: L'identifieur: '" + identifier.value + "' n'existe pas dans la portée actuelle.")
      case literal: Literal => literal match {
        case StringLiteral(value) => StringValue(value)
        case Number(value) => IntegerValue(value)
        case BooleanLiteral(value) => BooleanValue(value)
      }
    }
  }

}
