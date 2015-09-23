package com.airtial.hellolisp

import org.junit.{Assert, Test}

class ExpTest {

  @Test
  def testCalc(): Unit = {
    val i1 = Exp(1)
    val i2 = Exp(2)
    val f1 = Exp(1.1)
    val f2 = Exp(2.2)
    val ff = Library("+").asInstanceOf[ExpFunction]
    Assert.assertEquals(Exp(3), i1 + i2)
    Assert.assertEquals(Exp(3.3), f1 + f2)
    Assert.assertEquals(Exp(2.1), i1 + f1)
    Assert.assertEquals(Exp(2.1), f1 + i1)
    Assert.assertEquals(Exp(2.1), ff(List(i1, f1)))
    Assert.assertEquals(Exp(2.1), ff call List(i1, f1))
  }

  @Test
  def testCategorize(): Unit = {
    val actual = List(Exp("1024"), Exp("1.23"), Exp("aoe"))
    val expected = List(new ExpInt(1024), new ExpFloat(1.23), new ExpSymbol("aoe"))
    val isEqual: Boolean =
      actual.length == expected.length &&
        actual.indices
          .forall(index => actual(index) == expected(index))
    Assert.assertTrue(isEqual)
  }

}
