package wheels.airt.interpreter

import junit.framework.TestCase
import org.junit.Assert

class ExprTest extends TestCase {

  def testCat() {
    val actual = Expr.list(Expr.cat("1024"), Expr.cat("1.23"), Expr.cat("aoe"))
    val expected = Expr.list(ExprNumeric(1024), ExprNumeric(1.23), ExprIdentifier("aoe"))
    Assert.assertEquals(expected, actual)
  }

}
