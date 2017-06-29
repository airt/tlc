package wheels.airt.interpreter

import junit.framework.TestCase
import org.junit.Assert

class EvalTest extends TestCase {

  def testEval() {
    val code =
      """
        ((def foo (x) (+ x 1))
        ((lambda (x) (* x (foo x))) 2))
      """
    val actual = Main.eval(code)
    val expected = ExprNumeric(6)
    Assert.assertEquals(expected, actual)
  }

}
