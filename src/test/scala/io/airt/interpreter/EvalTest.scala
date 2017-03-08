package io.airt.interpreter

import org.junit.{Assert, Test}

class EvalTest {

  @Test
  def testEval(): Unit = {
    val code = """((defun foo (x) (+ x 1))
                   ((lambda (x) (* x (foo x))) 2))"""
    val actual = TLI.eval(code)
    val expected = Exp(6)
    Assert.assertEquals(expected, actual)
  }

  @Test
  def testEvalFromFile(): Unit = {
    val filepath = """src/test/resources/hl.l"""
    TLI.main(Array(filepath))
  }

}
