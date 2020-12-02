package fr.emalios.algointerpreter.parser

import fr.emalios.algointerpreter.token._

sealed trait AlgoAST
sealed trait Expression extends AlgoAST
sealed trait Instruction extends AlgoAST

case class IfThenElseInstruction(condition: Expression, thenBlock: Block, elseBlock: Option[Block])
case class Block(instructions: List[Instruction]) extends AlgoAST
case class BinaryOperation(leftExpression: Expression, operator: Operator, rightExpression: Expression) extends Expression
case class UnaryOperation(operator: Operator, right: Expression) extends Expression
case class Literal(value: String) extends Expression
case class Number(value: Int) extends Expression
case class Identifier(value: String) extends Expression