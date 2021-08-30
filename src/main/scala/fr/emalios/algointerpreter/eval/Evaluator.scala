package fr.emalios.algointerpreter.eval

import fr.emalios.algointerpreter.parser.{Assignment, ExprInstr, ForInstruction, IfThenElseInstruction, Instruction, Program, Return, WhileInstruction}

class Evaluator {

  /**
   * Méthode exécutant le programme donné sous la forme d'un ast qui doit être entièrement typé en entrée
   * @param program programme à exécuter
   */
  def eval(program: Program): Unit = program.mainAlgo.block.instructions.foreach(eval)

  /**
   * Méthode exécutant une instruction donné en entrée
   * @param instr instruction à exécuter
   */
  def eval(instr: Instruction): Unit = {
    instr match {
      case IfThenElseInstruction(condition, thenBlock, elseBlock) =>
      case Assignment(identifier, expression) =>
      case Return(expression) =>
      case ForInstruction(identifier, expressionFrom, expressionTo, block) =>
      case WhileInstruction(condition, block) =>
      case ExprInstr(e) =>
    }
  }

}
