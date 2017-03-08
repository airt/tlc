package io.airt.interpreter

import org.junit.{Assert, Test}

class InterpreterTest {

  @Test
  def testInterpretLet(): Unit = {
    val code = """(let ((x 1)
                        (y 2)
                        (z 3))
                       (+ x (* y z)))"""
    val actual = Interpreter.interpret(Parser.parse(code))
    val expected = Exp(7)
    Assert.assertEquals(expected, actual)
  }

  @Test
  def testInterpretLambda(): Unit = {
    val code = """((lambda (func x)
                             (func x (+ x 1)))
                           (lambda (x y)
                             (+ x y))
                           512)"""
    val actual = Interpreter.interpret(Parser.parse(code))
    val expected = Exp(1025)
    Assert.assertEquals(expected, actual)
  }

  @Test
  def testInterpretDefun(): Unit = {
    val code = """((defun foo (x) (+ x 1))
                   (foo 2))"""
    val actual = Interpreter.interpret(Parser.parse(code))
    val expected = Exp(3)
    Assert.assertEquals(expected, actual)
  }

  @Test
  def testInterpretQuote(): Unit = {
    val code = """(cdr '(1 2 3))"""
    val actual = Interpreter.interpret(Parser.parse(code))
    val expected = Exp(List(Exp(2), Exp(3)))
    Assert.assertEquals(expected, actual)
  }

  @Test
  def testInterpretIf(): Unit = {
    val code1 = """(if t (+ 1 (+ 2 3)) (+ 2 3))"""
    val actual1 = Interpreter.interpret(Parser.parse(code1))
    val expected1 = Exp(6)
    Assert.assertEquals(expected1, actual1)
    val code = """(if nil (+ 1 (+ 2 3)) (+ 2 3))"""
    val actual = Interpreter.interpret(Parser.parse(code))
    val expected = Exp(5)
    Assert.assertEquals(expected, actual)
  }

  @Test
  def testInterpretSimple(): Unit = {
    val code = """(* (+ 1 2) 3)"""
    val actual = Interpreter.interpret(Parser.parse(code))
    val expected = Exp(9)
    Assert.assertEquals(expected, actual)
  }

}
