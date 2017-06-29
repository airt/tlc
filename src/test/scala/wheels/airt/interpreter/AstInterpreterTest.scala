package wheels.airt.interpreter

import junit.framework.TestCase
import org.junit.Assert

class AstInterpreterTest extends TestCase {

  def testInterpretSimple() {
    val code = """(* (+ 1 2) 3)"""
    val actual = AstInterpreter.interpret(Parser.parse(code))
    val expected = ExprNumeric(9)
    Assert.assertEquals(expected, actual)
  }

  def testInterpretQuote() {
    val code = """'(1 2 3)"""
    val actual = AstInterpreter.interpret(Parser.parse(code))
    val expected = Expr.list(ExprNumeric(1), ExprNumeric(2), ExprNumeric(3))
    Assert.assertEquals(expected, actual)
  }

  def testInterpretIf() {
    val code1 = """(if t (+ 1 (+ 2 3)) (+ 2 3))"""
    val actual1 = AstInterpreter.interpret(Parser.parse(code1))
    val expected1 = ExprNumeric(6)
    Assert.assertEquals(expected1, actual1)
    val code = """(if nil (+ 1 (+ 2 3)) (+ 2 3))"""
    val actual = AstInterpreter.interpret(Parser.parse(code))
    val expected = ExprNumeric(5)
    Assert.assertEquals(expected, actual)
  }

  def testInterpretLet() {
    val code =
      """
        (let ((x 1)
              (y 2)
              (z 3))
          (+ x (* y z)))
      """
    val actual = AstInterpreter.interpret(Parser.parse(code))
    val expected = ExprNumeric(7)
    Assert.assertEquals(expected, actual)
  }

  def testInterpretLambda() {
    val code =
      """
        ((lambda (func x)
          (func x (+ x 1)))
          (lambda (x y)
          (+ x y))
          512)
      """
    val actual = AstInterpreter.interpret(Parser.parse(code))
    val expected = ExprNumeric(1025)
    Assert.assertEquals(expected, actual)
  }

  def testInterpretDef() {
    val code =
      """
        ((def foo (x) (+ x 1))
          (foo 2))"""
    val actual = AstInterpreter.interpret(Parser.parse(code))
    val expected = ExprNumeric(3)
    Assert.assertEquals(expected, actual)
  }

}
