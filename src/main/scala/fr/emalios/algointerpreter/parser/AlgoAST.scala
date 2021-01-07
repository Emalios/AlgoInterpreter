package fr.emalios.algointerpreter.parser

import fr.emalios.algointerpreter.eval.{Type, Value}
import fr.emalios.algointerpreter.token._

sealed trait AlgoAST
sealed trait Expression extends AlgoAST
sealed trait Literal extends Expression
sealed trait Instruction extends AlgoAST

case class Program(mainAlgo: Algo, declaredFunction: List[Function]) extends AlgoAST
case class Algo(block: Block) extends AlgoAST
case class IfThenElseInstruction(condition: Expression, thenBlock: Block, elseBlock: Option[Block]) extends Instruction
case class Block(instructions: List[Instruction]) extends AlgoAST
case class Affectation(identifier: Identifier, expression: Expression) extends Instruction
case class ForInstruction(identifier: Identifier, expressionFrom: Expression, expressionTo: Expression, block: Block) extends Instruction
case class BinaryOperation(leftExpression: Expression, operator: Token, rightExpression: Expression) extends Expression
case class UnaryOperation(operator: Token, right: Expression) extends Expression
case class StringLiteral(value: String) extends Literal
case class Number(value: Int) extends Literal
case class BooleanLiteral(value: Boolean) extends Literal
case class Identifier(value: String) extends Expression
case class Function(declaration: FunctionDeclaration, algo: Algo) extends AlgoAST
case class ReturnType(returnType: Type) extends AlgoAST
case class TypeParameter(name: Identifier, paramType: Type) extends AlgoAST
case class FunctionCall(functionName: Identifier, args: List[Expression]) extends Expression
case class FunctionDeclaration(functionName: Identifier, typeParameters: List[TypeParameter], returnType: Option[Type])
case class ExprInstr(e: Expression) extends Instruction