package fr.emalios.algointerpreter.eval

import fr.emalios.algointerpreter.parser
import fr.emalios.algointerpreter.parser.{Affectation, BinaryOperation, BooleanLiteral, Expression, Identifier, Instruction, Literal, Number, StringLiteral, UnaryOperation}
import fr.emalios.algointerpreter.token.{Minus, Mul, Not, Plus}

import scala.collection.mutable

class ASTEvaluator {

  private val env = new mutable.HashMap[Identifier, Literal]()

  def printEnv(): Unit = {
    printDelimiter(16)
    env.foreachEntry((identifier, expression) => {
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
      case Affectation(identifier, expression) => this.env.update(identifier, this.eval(expression))
    }
  }

  private def eval(expression: Expression): Literal = {
    expression match {
      case UnaryOperation(operator, right) => operator match {
        case Minus => Number(eval(right) match {
          case Number(value) => -value
        })
        case Not => BooleanLiteral(right match {
          case literal: Literal => literal match {
            case BooleanLiteral(value) => if (value == "vrai") "faux" else "vrai"
          }
        })
      }
      case BinaryOperation(left, operator, right) => operator match {
        case Plus => eval(left) match {
          case left: Number => eval(right) match {
            case right: Number => Number(left.value + right.value)
          }
        }
        case Minus => eval(left) match {
          case left: Number => eval(right) match {
            case right: Number => Number(left.value - right.value)
          }
        }
        case Mul => eval(left) match {
          case left: Number => eval(right) match {
            case right: Number => Number(left.value * right.value)
          }
        }
      }
      case identifier: Identifier => {
        if (this.env.contains(identifier)) this.env.get(identifier) match {
          case Some(value) => value match {
            case StringLiteral(value) => StringLiteral(value)
            case Number(value) => Number(value)
            case BooleanLiteral(value) => BooleanLiteral(value)
          }
        } else throw AlgoEvaluationError("Identifier: " + identifier + " doesn't exist in current scope.")
      }
      case literal: Literal => literal match {
        case StringLiteral(value) => StringLiteral(value)
        case parser.Number(value) => Number(value)
        case BooleanLiteral(value) => BooleanLiteral(value)
      }
    }
  }

}
