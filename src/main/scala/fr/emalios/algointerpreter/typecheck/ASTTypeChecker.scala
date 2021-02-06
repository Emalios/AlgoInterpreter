package fr.emalios.algointerpreter.typecheck

import fr.emalios.algointerpreter.eval.{AlgoTypeCheckingError, BooleanValue, CharValue, FunctionApplication, IntegerValue, Quantifier, RealValue, StringValue, Value}
import fr.emalios.algointerpreter.parser.{Algo, Assignment, BinaryOperation, Block, BooleanLiteral, ExprInstr, Expression, ForInstruction, Function, FunctionCall, FunctionDeclaration, Identifier, IfThenElseInstruction, Literal, Number, Program, Return, StringLiteral, TypeParameter, UnaryOperation, WhileInstruction}
import fr.emalios.algointerpreter.token
import fr.emalios.algointerpreter.token.{Affectation, And, CharTypeToken, Comma, Do, Dot, DoublePoints, Else, End, EndFor, EndIf, EndOfLine, EndWhile, Equals, False, For, From, Function, Greater, GreaterEqual, If, In, InOut, IntegerTypeToken, LEFT_BOX_BRACKET, LeftParen, Lesser, LesserEqual, Minus, Mod, Mul, Not, NotEquals, Or, Out, Percent, Plus, RIGHT_BOX_BRACKET, RealTypeToken, RightParen, Slash, Start, StartLoop, StringTypeToken, Then, To, True, While}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ASTTypeChecker {

  private def reset() = {
    this.globalFrame.clear()
    this.globalFrame.addOne((Identifier("ecrire"), FunctionType(Option(Seq(TypeParameter(Identifier("to_print"), AnyType(), fr.emalios.algointerpreter.eval.In))), Option.empty)))
    this.stack.clear()
    this.stack += new mutable.HashMap[Identifier, Type]()
  }

  def print(): Unit = {
    this.stack.foreach(frame => frame.foreachEntry((identifier, typeP) => println("    " + identifier + "->" + typeP)))
  }

  type Frame = mutable.HashMap[Identifier, Type]
  type Stack = mutable.ArrayBuffer[Frame]
  private var stack: Stack = mutable.ArrayBuffer[Frame]()
  private val globalFrame: Frame = mutable.HashMap[Identifier, Type]()

  def typecheckExpression(expression: Expression): Type = {
    expression match {
      case literal: Literal => literal match {
        case StringLiteral(_) => StringType
        case Number(_) => IntegerType
        case BooleanLiteral(_) => BooleanType
      }
      case BinaryOperation(leftExpression, operator, rightExpression) =>
        val expressionType = this.unify(leftExpression, rightExpression)
        operator match {
          case GreaterEqual => BooleanType
          case LesserEqual => BooleanType
          case Lesser => BooleanType
          case Greater => BooleanType
          case Equals => BooleanType
          case _ => expressionType
        }
      case UnaryOperation(_, right) => this.typecheckExpression(right)
      case FunctionCall(functionName, _) =>
        if(!this.globalFrame.contains(functionName)) throw AlgoTypeCheckingError("Erreur: La fonction '" + functionName.value + "'' n'existe pas.")
        this.globalFrame(functionName) match {
          case FunctionType(_, returnType) => returnType.get
        }
      case identifier: Identifier =>
        if(this.getCurrentFrame.contains(identifier)) this.getCurrentFrame(identifier)
        else throw AlgoTypeCheckingError("Pas censé arriver")
    }
  }

  /**
   * Method who throw an error if expression's type are not equals to expected type
   * @param expression expression to test the type
   * @param expectedType expected type for expression
   */
  def typecheckExpression(expression: Expression, expectedType: Type): Unit = {
    val expressionType = this.typecheckExpression(expression)
    if(!expressionType.equals(expectedType)) this.typecheckExpressionError(expression, expectedType)
  }

  private def typecheckExpressionError(expression: Expression, expectedType: Type): Unit = {
    throw AlgoTypeCheckingError("Erreur: Mauvais type pour l'expression '" + expression + "'. " + expectedType + " attendue.")
  }

  private def typecheckUnifyError(expression1: Expression, expression2: Expression, type1: Type, type2: Type): Unit = {
    throw AlgoTypeCheckingError("Erreur: '" + expression1 + "'(" + type1 + ") n'est pas du même type que '" + expression2 + "'(" + type2 + "')")
  }

  def unify(expression1: Expression, expression2: Expression): Type = {
    val type1 = this.typecheckExpression(expression1)
    val type2 = this.typecheckExpression(expression2)
    if (!type1.equals(type2)) this.typecheckUnifyError(expression1, expression2, type1, type2)
    type1
  }

  def typecheckAST(ast: Program): Unit = {
    this.reset()
    ast.declaredFunction.foreach {
      case fr.emalios.algointerpreter.parser.Function(declaration, algo) => this.typecheckFunctionDeclaration(declaration, algo)
    }
    //typecheck main algo block
    this.typecheckBlock(ast.mainAlgo.block, Option.empty)
  }

  private def typecheckFunctionDeclaration(declaration: FunctionDeclaration, algo: Algo): Unit = {
    //add required types for functions in global frame
    val identifier = declaration.functionName
    if(this.globalFrame.contains(identifier)) throw AlgoTypeCheckingError("Erreur: La fonction '" + identifier.value + "' existe déjà.")
    this.globalFrame.addOne((identifier, declaration.functionType))
    val frame = this.getCurrentFrame.clone()
    //add parameters to current frame to eval function block
    declaration.functionType.parametersType match {
      case Some(value) => value.foreach(typeParameter => frame.addOne((typeParameter.name, typeParameter.paramType)))
      case None =>
    }
    //push clone of current frame
    this.stack += frame
    //typecheck function block
    this.typecheckBlock(algo.block, Option(identifier))
    //pop frame
    this.stack.remove(this.stack.length-1)
  }

  private def typecheckFunctionCall(functionName: Identifier, args: List[Expression]): Unit = {
    if(!this.globalFrame.contains(functionName)) throw AlgoTypeCheckingError("Erreur: La fonction '" + functionName.value + "'' n'existe pas.")
    val parametersType: Seq[Type] = this.globalFrame(functionName) match {
      case functionType: FunctionType => functionType.parametersType.get.map(parametersType => parametersType.paramType)
    }
    //check if there are enough arguments in args
    //TODO: remake error message
    if(args.length != parametersType.length) throw AlgoTypeCheckingError("Erreur: Pas assez d'arguments fournis pour la fonction '" + functionName.value + "'.")
    //typecheck all arguments
    for((typeParameter, value) <- parametersType zip args ) {
      this.typecheckExpression(value, typeParameter)
    }
  }

  private def typecheckBlock(block: Block, functionName: Option[Identifier]): Unit = {
    val currentFrame = this.getCurrentFrame
    block.instructions.foreach {
      case IfThenElseInstruction(condition, thenBlock, elseBlock) =>
        //first, we typecheck condition as a boolean type
        //TODO: eventually add a method who uses type parameter to avoid instantiation of useless type
        this.typecheckExpression(condition, BooleanType)
        //push
        this.stack += this.getCurrentFrame.clone()
        this.typecheckBlock(thenBlock, functionName)
        //pop
        this.stack.remove(this.stack.length - 1)
        elseBlock match {
          case Some(value) =>
            //push
            this.stack += this.getCurrentFrame.clone()
            this.typecheckBlock(value, functionName)
            //pop
            this.stack.remove(this.stack.length - 1)
          case None =>
        }
      case Assignment(identifier, expression) =>
        if(currentFrame.contains(identifier)) this.typecheckExpression(expression, currentFrame(identifier))
        else currentFrame.addOne((identifier, this.typecheckExpression(expression)))
      case Return(expression) =>
        val name = functionName match {
          case Some(value) => value
          case None => throw AlgoTypeCheckingError("")
        }
        val returnType: Type = this.globalFrame(name) match {
          case FunctionType(_, returnType) => returnType match {
            case Some(value) => value
            case None => throw AlgoTypeCheckingError("Erreur: 'retourne' détecté dans une fonction ne retournant rien.")
          }
        }
        this.typecheckExpression(expression, returnType)
      case ForInstruction(identifier, expressionFrom, expressionTo, block) =>
        //push
        this.stack += this.getCurrentFrame.clone()
        //update identifier in frame as IntegerType
        this.getCurrentFrame.update(identifier, IntegerType)
        val expectedType = IntegerType
        //typecheck expressionFrom and expressionTo as IntegerType
        this.typecheckExpression(expressionFrom, expectedType)
        this.typecheckExpression(expressionTo, expectedType)
        //now we can typecheck block
        this.typecheckBlock(block, functionName)
        //pop
        this.stack.remove(this.stack.length - 1)
      case WhileInstruction(condition, block) =>
        //typecheck condition as boolean type
        this.typecheckExpression(condition, BooleanType)
        //push
        this.stack += this.getCurrentFrame.clone()
        //now we can typecheck block
        this.typecheckBlock(block, functionName)
        //pop
        this.stack.remove(this.stack.length - 1)
      case ExprInstr(e) => e match {
        case FunctionCall(functionName, args) =>
          //typecheck arguments
          this.typecheckFunctionCall(functionName, args)
      }
    }
  }

  private def getCurrentFrame: Frame = this.stack.last

}
