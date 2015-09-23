package com.airtial.hellolisp

import org.junit.{Assert, Test}

class ReplTest {

  @Test
  def testEval(): Unit = {
    val code = """((defun foo (x) (+ x 1))
                   ((lambda (x) (* x (foo x))) 2))"""
    val actual = Hellolisp.eval(code)
    val expected = Exp(6)
    Assert.assertEquals(expected, actual)
  }

  @Test
  def testEvalFromFile(): Unit = {
    val filepath = """src/test/resources/hl.l"""
    Hellolisp.main(Array(filepath))
  }

}
