package fr.emalios.algointerpreter.eval

sealed trait Quantifier

case object In extends Quantifier
case object Out extends Quantifier
case object InOut extends Quantifier
