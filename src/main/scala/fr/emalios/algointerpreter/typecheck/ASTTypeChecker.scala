package fr.emalios.algointerpreter.typecheck

import fr.emalios.algointerpreter.parser._
import fr.emalios.algointerpreter.token.{Equals, Greater, GreaterEqual, Less, LesserEqual, Identifier => _, Literal => _}

import scala.collection.mutable

class ASTTypeChecker {
/*
  private def reset(): Stack = {
    this.globalFrame.clear()
    this.globalFrame.addOne((Identifier("ecrire"), FunctionType(Option(Seq(TypeParameter(Identifier("to_print"), AnyType(), fr.emalios.algointerpreter.eval.In))), Option.empty)))
    this.globalFrame.addOne((Identifier("lire"), FunctionType(Option.empty, Option(Undefined))))
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

  /**
   * Method who throw an error if expression's type are not equals to expected type
   * @param expression expression to test the type
   * @param expectedType expected type for expression
   */
  def typecheckExpression(expression: Expression, expectedType: Type): Unit = {
    val expressionType = this.typeOf(expression)
    if(!expressionType.equals(expectedType)) this.typecheckExpressionError(expression, expectedType)
  }

  private def typecheckExpressionError(expression: Expression, expectedType: Type): Unit = {
    throw AlgoTypeCheckingError("Erreur: Mauvais type pour l'expression '" + expression + "'. " + expectedType + " attendue.")
  }

  private def typecheckUnifyError(expression1: Expression, expression2: Expression, type1: Type, type2: Type): Unit = {
    throw AlgoTypeCheckingError("Erreur: '" + expression1 + "'(" + type1 + ") n'est pas du même type que '" + expression2 + "'(" + type2 + "')")
  }

  /**
   * Method which assert that the types of two given expressions are the same and return it
   * @param expression1 first expression
   * @param expression2 other expression
   * @return the type of all expressions
   */
  def unify(expression1: Expression, expression2: Expression): Type = {
    val type1 = this.typeOf(expression1)
    val type2 = this.typeOf(expression2)
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
        else currentFrame.addOne((identifier, this.typeOf(expression)))
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

 */

}
