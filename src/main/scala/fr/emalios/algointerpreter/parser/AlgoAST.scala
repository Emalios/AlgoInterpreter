package fr.emalios.algointerpreter.parser

import fr.emalios.algointerpreter.token._

sealed trait AlgoAST
sealed trait Expression extends AlgoAST
sealed trait Instruction extends AlgoAST

case class Algo(block: Block) extends AlgoAST
case class IfThenElseInstruction(condition: Expression, thenBlock: Block, elseBlock: Option[Block]) extends Instruction
case class Block(instructions: List[Instruction]) extends AlgoAST
case class Affectation(identifier: Identifier, expression: Expression) extends Instruction
case class ForInstruction(identifier: Identifier, expressionFrom: Expression, expressionTo: Expression, block: Block) extends Instruction
case class BinaryOperation(leftExpression: Expression, operator: Operator, rightExpression: Expression) extends Expression
case class UnaryOperation(operator: Operator, right: Expression) extends Expression
case class Literal(value: String) extends Expression
case class Number(value: Int) extends Expression
case class Identifier(value: String) extends Expression