package wheels.airt.interpreter

import junit.framework.TestCase
import org.junit.Assert

class ParserTest extends TestCase {

  def testParse() {
    val code = """(foo 0 (+ 1 (bar "aoe")))"""
    val actual = Parser.parse(code)
    val expected =
      Expr.list(
        ExprIdentifier("foo"), ExprNumeric(0),
        Expr.list(
          ExprIdentifier("+"), ExprNumeric(1),
          Expr.list(
            ExprIdentifier("bar"),
            ExprString("aoe")
          )
        )
      )
    Assert.assertEquals(expected, actual)
  }

  def testParseQuote() {
    val code = """(foo '(1 2 3) '(4 5 6) '(7 8 9))"""
    val actual = Parser.parse(code)
    val expected =
      Expr.list(ExprIdentifier("foo"),
        Expr.list(ExprIdentifier("quote"),
          Expr.list(ExprNumeric(1), ExprNumeric(2), ExprNumeric(3))
        ),
        Expr.list(ExprIdentifier("quote"),
          Expr.list(ExprNumeric(4), ExprNumeric(5), ExprNumeric(6))
        ),
        Expr.list(ExprIdentifier("quote"),
          Expr.list(ExprNumeric(7), ExprNumeric(8), ExprNumeric(9))
        )
      )
    Assert.assertEquals(expected, actual)
  }

  def testTokenize() {
    val code = """(foo 0 1)"""
    val actual = Parser.tokenize(code)
    val expected = List(
      ExprIdentifier("("),
      ExprIdentifier("("),
      ExprIdentifier("foo"),
      ExprNumeric(0),
      ExprNumeric(1),
      ExprIdentifier(")"),
      ExprIdentifier(")")
    )
    Assert.assertEquals(expected, actual)
  }

}
