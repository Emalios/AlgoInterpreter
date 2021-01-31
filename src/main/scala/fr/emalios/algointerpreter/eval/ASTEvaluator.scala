package fr.emalios.algointerpreter.eval

import com.ibm.jvm.Trace
import fr.emalios.algointerpreter.Main.devDebugMode
import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.parser._
import fr.emalios.algointerpreter.parser.{Affectation, BinaryOperation, Block, BooleanLiteral, ExprInstr, Expression, FunctionCall, Identifier, Instruction, Literal, Number, StringLiteral, UnaryOperation}
import fr.emalios.algointerpreter.token.{And, Equals, Greater, GreaterEqual, Lesser, LesserEqual, Minus, Mul, Not, NotEquals, Or, Plus}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
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
    if (devDebugMode) println("add functions")
    program.declaredFunction.foreach(this.addFunctionToGlobalScope)
    if (devDebugMode) println(this.getCurrentFrame)
    this.evalBlock(program.mainAlgo.block)
  }

  def addFunctionToGlobalScope(function: Function): Unit = {
    if (devDebugMode) println("Ajout de la fonction: " + function.declaration.functionName + " au scope global.")
    this.callStack.head.addOne((function.declaration.functionName, (FunctionApplication(function.declaration, function.algo.block), In)))
  }

  def evalInstruction(instruction: Instruction): Option[Value] =
    instruction match {
      case Affectation(identifier, expression) =>
        if (this.getCurrentFrame.contains(identifier)) this.getCurrentFrame(identifier)._2 match {
          case In => throw AlgoEvaluationError("Erreur: Vous ne pouvez pas modifier la variable '" + identifier.value + "'")
          case _ =>
            val expressionValue = evalExpression(expression)
            val identifierValue = this.getCurrentFrame(identifier)._1
            identifierValue match {
              case actualValue: BooleanValue => expressionValue match {
                case oldValue: BooleanValue => actualValue.value.set(oldValue.value.get()); Option.empty
                case _ => throw AlgoTypeCheckingError("Erreur: booléen attendu pour '" + expressionValue + "'")
              }
              case actualValue: IntegerValue => expressionValue match {
                case oldValue: IntegerValue => actualValue.value.set(oldValue.value.get()); Option.empty
                case _ => throw AlgoTypeCheckingError("Erreur: entier attendu pour '" + expressionValue + "'")
              }
            }
        } else {
          this.getCurrentFrame.addOne(identifier, (this.evalExpression(expression), InOut))
          Option.empty
        }
      case exprInstr: ExprInstr => exprInstr.e match {
        case FunctionCall(functionName, args) => functionName match {
          case _ => callFunction(functionName, args); Option.empty
        }
      }
      case ForInstruction(identifier, expressionFrom, expressionTo, block) =>
        //typecheck expressionFrom and expressionTo, must be integers
        val evaluatedExpressionFrom = this.evalExpression(expressionFrom)
        evaluatedExpressionFrom match {
          case valueFrom: IntegerValue =>
            val evaluatedExpressionTo = this.evalExpression(expressionTo)
            evaluatedExpressionTo match {
              case valueTo: IntegerValue =>
                val from = valueFrom.value.get()
                val to = valueTo.value.get()
                //from must be < to
                if (from > to) throw AlgoEvaluationError("Erreur: " + from + " < " + to + "")
                val frame: Frame = this.getCurrentFrame.clone()
                val integerValue = IntegerValue(new AtomicInteger(valueFrom.value.get()))
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
                this.callStack.remove(this.callStack.length-1)
                Option.empty
              case _ => throw AlgoTypeCheckingError("Erreur: entier attendu pour '" + evaluatedExpressionTo + "'")
            }
          case _ => throw AlgoTypeCheckingError("Erreur: entier attendu pour '" + evaluatedExpressionFrom + "'")
        }
      case WhileInstruction(condition, block) =>
        def evaluatedCondition = this.evalExpression(condition)
        //update callstack (push)
        val frame: Frame = this.getCurrentFrame.clone()
        this.callStack.addOne(frame)
        //eval while instruction
        while (evaluatedCondition match {
          //typecheck, condition must be a boolean
          case value: BooleanValue => value.value.get()
          case _ => throw AlgoTypeCheckingError("Erreur: booléen attendu pour '" + evaluatedCondition)
        }) {
          this.evalBlock(block)
        }
        //pop
        this.callStack.remove(this.callStack.length-1)
        Option.empty
      case IfThenElseInstruction(condition, thenBlock, elseBlock) => this.evalExpression(condition) match {
        //TODO: refactor type checker to remove redudant code.
        case BooleanValue(value) =>
          val frame: Frame = this.getCurrentFrame.clone()
          var blockValue: Option[Value] = null
          this.callStack.addOne(frame)
          if (value.get()) {
            blockValue = this.evalBlock(thenBlock)
            this.callStack.remove(this.callStack.length-1)
            blockValue
          } else if (elseBlock.nonEmpty) {
            blockValue = this.evalBlock(elseBlock.get)
            this.callStack.remove(this.callStack.length-1)
            blockValue
          } else
            Option.empty
        case _ => /* error */ throw AlgoTypeCheckingError("Erreur: booléen attendu pour une condition '" + condition + "'.")
      }
        //TODO: remove declared variables from if then else block.
      case Return(expression) => Option(this.evalExpression(expression))
    }

  private def callFunction(functionName: Identifier, values: List[Expression]): Option[Value] = {
    if (this.getCurrentFrame.contains(functionName)) {
      this.getCurrentFrame(functionName) match {
        case (PrimFunction(function), _) => Option(function.apply(values.map(evalExpression)))
        case (FunctionApplication(declaration, block), _) =>
          //if number of elements in values mismatch with the number of elements in function declaration, error
          if(values.length != declaration.typeParameters.length) throw AlgoEvaluationError("Erreur: La fonction '" + functionName + "' prend " + declaration.typeParameters.length + " paramètre(s), ici, " + values.length + " argument(s) lui sont fournis.")
          //now we can typecheck gived arguments with inference
          val frame: Frame = this.callStack.head.clone()
          for((typeParameter, value) <- declaration.typeParameters zip values) {
            val expressionValue = evalExpression(value)
            typeParameter.paramType match {
              case BooleanType => expressionValue match {
                case value: BooleanValue => frame.addOne(typeParameter.name, (value, typeParameter.quantifier))
                case _ => this.typeCheckError("booléen", typeParameter, value)
              }
              case RealType => expressionValue match {
                case value: RealValue => frame.addOne(typeParameter.name, (value, typeParameter.quantifier))
                case _ => this.typeCheckError("réel", typeParameter, value)
              }
              case StringType => expressionValue match {
                case value: StringValue => frame.addOne(typeParameter.name, (value, typeParameter.quantifier))
                case _ => this.typeCheckError("chaine", typeParameter, value)
              }
              case CharType => expressionValue match {
                case value: CharValue => frame.addOne(typeParameter.name, (value, typeParameter.quantifier))
                case _ => this.typeCheckError("charactere", typeParameter, value)
              }
              case IntegerType => expressionValue match {
                case value: IntegerValue => frame.addOne(typeParameter.name, (value, typeParameter.quantifier))
                case _ => this.typeCheckError("entier", typeParameter, value)
              }
            }
          }
          this.callStack.addOne(frame)
          val value = evalBlock(block)
          this.callStack.remove(this.callStack.length-1)
          value
      }
    }
    else throw AlgoEvaluationError("Erreur: La fonction '" + functionName + "' n'existe pas.")
  }

  def typeCheckError(expectedType: String, typeParameter: TypeParameter, value: Expression): Unit = {
    throw AlgoTypeCheckingError("Erreur: " + expectedType + " attendu pour le paramètre " + typeParameter + ". (" + value + ") obtenue")
  }

  def evalBlock(block: Block): Option[Value] = {
    var i = 0
    var returnDetected = false
    while (i < block.instructions.length && !returnDetected) {
      block.instructions(i) match {
        case Return(_) => returnDetected = true
        case _ => this.evalInstruction(block.instructions(i)); i+=1
      }
    }
    if(returnDetected) this.evalInstruction(block.instructions(i)) else Option.empty
  }

  private def evalExpression(expression: Expression): Value = {
    expression match {
      case FunctionCall(functionName, args) => /* todo, check if value is preset */ this.callFunction(functionName, args).get
      case UnaryOperation(operator, right) => operator match {
        case Minus => IntegerValue(evalExpression(right) match {
          case IntegerValue(value) => new AtomicInteger(-value.get())
        })
        case Not => BooleanValue(evalExpression(right) match {
            case BooleanValue(value) => value
        })
      }
      case BinaryOperation(left, operator, right) => operator match {
        case Plus => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => IntegerValue(new AtomicInteger(left.value.get() + right.value.get()))
          }
        }
        case Minus => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => IntegerValue(new AtomicInteger(left.value.get() - right.value.get()))
          }
        }
        case Mul => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => IntegerValue(new AtomicInteger(left.value.get() * right.value.get()))
          }
        }
        case Lesser => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(new AtomicBoolean(left.value.get() < right.value.get()))
          }
        }
        case LesserEqual => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(new AtomicBoolean(left.value.get() <= right.value.get()))
          }
        }
        case Greater => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(new AtomicBoolean(left.value.get() > right.value.get()))
          }
        }
        case GreaterEqual => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(new AtomicBoolean(left.value.get() >= right.value.get()))
          }
        }
        case Equals => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(new AtomicBoolean(left.value.get() == right.value.get()))
          }
        }
        case NotEquals => evalExpression(left) match {
          case left: IntegerValue => evalExpression(right) match {
            case right: IntegerValue => BooleanValue(new AtomicBoolean(left.value.get() != right.value.get()))
          }
        }
        case And => evalExpression(left) match {
          case left: BooleanValue => evalExpression(right) match {
            case right: BooleanValue => BooleanValue(new AtomicBoolean(left.value.get() && right.value.get()))
          }
        }
        case Or => evalExpression(left) match {
          case left: BooleanValue => evalExpression(right) match {
            case right: BooleanValue => BooleanValue(new AtomicBoolean(left.value.get() || right.value.get()))
          }
        }
      }
      case identifier: Identifier =>
        if (this.getCurrentFrame.contains(identifier)) this.getCurrentFrame(identifier)._2 match {
          case Out => throw AlgoEvaluationError("Erreur: Vous ne pouvez pas lire cette variable : '" + identifier.value + "'")
          case _ => this.getCurrentFrame(identifier)._1 match {
            case value: IntegerValue => value
            case value: RealValue => value
            case value: BooleanValue => value
            case value: StringValue => value
            case value: CharValue => value
          }
        }
        else throw AlgoEvaluationError("Erreur: L'identifieur: '" + identifier.value + "' n'existe pas dans la portée actuelle.")
      case literal: Literal => literal match {
        case StringLiteral(value) => StringValue(value)
        case Number(value) => IntegerValue(new AtomicInteger(value))
        case BooleanLiteral(value) => BooleanValue(new AtomicBoolean(value))
      }
    }
  }

}
