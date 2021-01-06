package fr.emalios.algointerpreter.eval

import fr.emalios.algointerpreter.lexer.{AlgoLexer, AlgoLexerError}
import fr.emalios.algointerpreter.parser.TokensParser
import fr.emalios.algointerpreter.parser.{Affectation, BinaryOperation, Block, BooleanLiteral, ExprInstr, Expression, FunctionCall, Identifier, Instruction, Literal, Number, StringLiteral, UnaryOperation}
import fr.emalios.algointerpreter.token.{And, Equals, Greater, GreaterEqual, Lesser, LesserEqual, Minus, Mul, Not, Or, Plus}

import java.util.Scanner
import scala.collection.mutable

class ASTEvaluator {

  private val varEnv = new mutable.HashMap[Identifier, Value]()

  def printEnv(): Unit = {
    printDelimiter(16)
    varEnv.foreachEntry((identifier, expression) => {
      println(identifier.value + "|" + expression)
    })
    printDelimiter(16)
  }

  private def printDelimiter(length: Int): Unit = {
    for (_ <- 1 to length) print("-")
    println()
  }

  def addInstructionToEnv(instruction: Instruction): Unit = {
    instruction match {
      case Affectation(identifier, expression) => this.varEnv.update(identifier, this.eval(expression))
      case exprInstr: ExprInstr => exprInstr.e match {
        case FunctionCall(functionName, args) => functionName match {
          case Identifier("ecrire") => args.foreach( expression => println(eval(expression)))
          case _ => if (this.varEnv.contains(functionName)) callFunction(functionName, args) else throw AlgoEvaluationError("Error: Function '" + functionName + "' do not exist.")
        }
      }
    }
  }

  private def callFunction(identifier: Identifier, value: List[Expression]): Unit = {
    this.varEnv(identifier) match {
      case FunctionValue(args, body) => {
        if (args.length != value.length) throw AlgoEvaluationError("Error: The parameters given to call the function '" + identifier + "' do not match what it request.")
        for ( i: Int <- args.indices) {

        }
      }
    }
  }

  private def eval(expression: Expression): Value = {
    expression match {
      case FunctionCall(functionName, args) => functionName match {
        case Identifier("lire") =>
          val scanner: java.util.Scanner = new java.util.Scanner(System.in)
          val input: String = scanner.nextLine()
          val lexer:AlgoLexer = new AlgoLexer
          val tokens = lexer.apply(input)
          val parser = new TokensParser
          val expression: Expression = parser.applyInput(tokens)
          eval(expression)
      }
      case UnaryOperation(operator, right) => operator match {
        case Minus => IntegerValue(eval(right) match {
          case IntegerValue(value) => -value
        })
        case Not => BooleanValue(eval(right) match {
            case BooleanValue(value) => value
        })
      }
      case BinaryOperation(left, operator, right) => operator match {
        case Plus => eval(left) match {
          case left: IntegerValue => eval(right) match {
            case right: IntegerValue => IntegerValue(left.value + right.value)
          }
        }
        case Minus => eval(left) match {
          case left: IntegerValue => eval(right) match {
            case right: IntegerValue => IntegerValue(left.value - right.value)
          }
        }
        case Mul => eval(left) match {
          case left: IntegerValue => eval(right) match {
            case right: IntegerValue => IntegerValue(left.value * right.value)
          }
        }
        case Lesser => eval(left) match {
          case left: IntegerValue => eval(right) match {
            case right: IntegerValue => BooleanValue(left.value < right.value)
          }
        }
        case LesserEqual => eval(left) match {
          case left: IntegerValue => eval(right) match {
            case right: IntegerValue => BooleanValue(left.value <= right.value)
          }
        }
        case Greater => eval(left) match {
          case left: IntegerValue => eval(right) match {
            case right: IntegerValue => BooleanValue(left.value > right.value)
          }
        }
        case GreaterEqual => eval(left) match {
          case left: IntegerValue => eval(right) match {
            case right: IntegerValue => BooleanValue(left.value >= right.value)
          }
        }
        case Equals => eval(left) match {
          case left: IntegerValue => eval(right) match {
            case right: IntegerValue => BooleanValue(left.value == right.value)
          }
        }
        case And => eval(left) match {
          case left: BooleanValue => eval(right) match {
            case right: BooleanValue => BooleanValue(left.value && right.value)
          }
        }
        case Or => eval(left) match {
          case left: BooleanValue => eval(right) match {
            case right: BooleanValue => BooleanValue(left.value || right.value)
          }
        }
      }
      case identifier: Identifier => {
        if (this.varEnv.contains(identifier)) this.varEnv.get(identifier) match {
          case Some(value) => value match {
            case literal => literal
            case IntegerValue(value) => IntegerValue(value)
            case BooleanValue(value) => BooleanValue(value)
          }
        } else throw AlgoEvaluationError("Identifier: " + identifier + " doesn't exist in current scope.")
      }
      case literal: Literal => literal match {
        case StringLiteral(value) => StringValue(value)
        case Number(value) => IntegerValue(value)
        case BooleanLiteral(value) => BooleanValue(value)
      }
    }
  }

}
