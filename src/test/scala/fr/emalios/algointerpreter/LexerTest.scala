package fr.emalios.algointerpreter

import fr.emalios.algointerpreter.lexer.AlgoLexer
import fr.emalios.algointerpreter.token._
import org.scalatest.{BeforeAndAfter, FunSuite}

class LexerTest extends FunSuite with BeforeAndAfter {

  val lexer = new AlgoLexer()

  test("minimalist program does not pass lexing") {
    val input = "Debut \nFin"
    val output = this.lexer.apply(input)
    assert(output === List(Start, EndOfLine, End))
  }

  test("whitespace are not ignored") {
    val input = "              Debut \n                            Fin"
    val output = this.lexer.apply(input)
    assert(output === List(Start, EndOfLine, End))
  }

  test("function call does not pass lexing") {
    var input = "Debut\n f() \nFin"
    def output: List[Token] = this.lexer.apply(input)
    assert(output === List(Start, EndOfLine, Identifier("f"), LeftParen, RightParen, EndOfLine, End))
    input = "Debut\n f(8, \"foo\",g(1)) \n Fin"
    assert(output === List(Start, EndOfLine, Identifier("f"), LeftParen, IntegerToken(8), Comma, StringToken("foo"), Comma, Identifier("g"), LeftParen, IntegerToken(1), RightParen, RightParen, EndOfLine, End))
  }

  test("assignment does not pass lexing") {
    var input = "Debut\n x <- 5 \nFin"
    def output: List[Token] = this.lexer.apply(input)
    assert(output === List(Start, EndOfLine, Identifier("x"), Affectation, IntegerToken(5), EndOfLine, End))
    input = "Debut\n x <- f() \n Fin"
    assert(output === List(Start, EndOfLine, Identifier("x"), Affectation, Identifier("f"), LeftParen, RightParen, EndOfLine, End))
  }

  test("if instruction does not pass lexing") {
    var input = "Debut\n si x = 5 alors \n x <- 1 \n fsi \nFin"
    def output: List[Token] = this.lexer.apply(input)
    assert(output === List(Start, EndOfLine, If, Identifier("x"), Equals, IntegerToken(5), Then, EndOfLine, Identifier("x"), Affectation, IntegerToken(1), EndOfLine, EndIf, EndOfLine, End))
    input = "Debut\n si x = 5 alors \n x <- 1 \n sinon \n x <- 2 \n fsi \n Fin"
    assert(output === List(Start, EndOfLine, If, Identifier("x"), Equals, IntegerToken(5), Then, EndOfLine, Identifier("x"), Affectation, IntegerToken(1), EndOfLine, Else, EndOfLine, Identifier("x"), Affectation, IntegerToken(2), EndOfLine, EndIf, EndOfLine, End))
  }

  test("for instruction does not pass lexing") {
    val input = "Debut\n pour i de 1 a 10 faire \n x <- 1 \n fpour \nFin"
    def output: List[Token] = this.lexer.apply(input)
    assert(output === List(Start, EndOfLine, For, Identifier("i"), From, IntegerToken(1), To, IntegerToken(10), Do, EndOfLine, Identifier("x"), Affectation, IntegerToken(1), EndOfLine, EndFor, EndOfLine, End))
  }

  test("while instruction does not pass lexing") {
    val input = "Debut\n tantque x != 3 faire\n x <- 1\n ftant\n Fin"
    def output: List[Token] = this.lexer.apply(input)
    assert(output === List(Start, EndOfLine, While, Identifier("x"), NotEquals, IntegerToken(3), Do, EndOfLine, Identifier("x"), Affectation, IntegerToken(1), EndOfLine, EndWhile, EndOfLine, End))
  }

  test("unary expressions do not pass lexing") {
    val input = "Debut\n x <- -5 \n y <- !faux \nFin"
    def output = this.lexer.apply(input)
    assert(output === List(Start, EndOfLine, Identifier("x"), Affectation, Minus, IntegerToken(5), EndOfLine, Identifier("y"), Affectation, Not, BooleanToken(false), EndOfLine, End))
  }

  test("binary expressions do not pass lexing") {
    val input = "Debut \n x <- 1+(1-1)*1/1%1\n y <- faux et vrai ou faux\n Fin"
    def output = this.lexer.apply(input)
    assert(output === List(Start, EndOfLine, Identifier("x"), Affectation, IntegerToken(1), Plus, LeftParen, IntegerToken(1), Minus, IntegerToken(1), RightParen, Mul, IntegerToken(1), Slash, IntegerToken(1), Percent, IntegerToken(1), EndOfLine, Identifier("y"), Affectation, BooleanToken(false), And, BooleanToken(true), Or, BooleanToken(false), EndOfLine, End))
  }

  test("function declaration does not pass lexing") {
    val input = "fonction f(x: entier, s: chaine, b: booleen): entier"
    def output = this.lexer.apply(input)
    assert(output === List(Function, Identifier("f"), LeftParen, Identifier("x"), DoublePoints, IntegerTypeToken, Comma, Identifier("s"), DoublePoints, StringTypeToken, Comma, Identifier("b"), DoublePoints, BooleanTypeToken, RightParen, DoublePoints, IntegerTypeToken))
  }

}
