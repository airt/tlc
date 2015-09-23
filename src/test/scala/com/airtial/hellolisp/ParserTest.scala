package com.airtial.hellolisp

import org.junit.{Assert, Test}

class ParserTest {

  @Test
  def testParse(): Unit = {
    val code = """(foo 0 (+ 1 (bar "aoe")))"""
    val actual = Parser.parse(code)
    val expected =
      new ExpList(List(new ExpSymbol("foo"), new ExpInt(0),
        new ExpList(List(
          new ExpSymbol("+"), new ExpInt(1),
          new ExpList(List(new ExpSymbol("bar"),
            new ExpString("aoe")))
        ))
      ))
    Assert.assertEquals(expected, actual)
  }

  @Test
  def testParseQuote(): Unit = {
    val code = """(foo '(1 2 3) '(4 5 6) '(7 8 9))"""
    val actual = Parser.parse(code)
    val expected =
      new ExpList(List(new ExpSymbol("foo"),
        new ExpList(List(new ExpSymbol("quote"),
          new ExpList(List(new ExpInt(1), new ExpInt(2), new ExpInt(3)))
        )),
        new ExpList(List(new ExpSymbol("quote"),
          new ExpList(List(new ExpInt(4), new ExpInt(5), new ExpInt(6)))
        )),
        new ExpList(List(new ExpSymbol("quote"),
          new ExpList(List(new ExpInt(7), new ExpInt(8), new ExpInt(9)))
        ))
      ))
    Assert.assertEquals(expected, actual)
  }

  @Test
  def testTokenize(): Unit = {
    val code = """(foo 0 1)"""
    val actual = Parser.tokenize(code)
    val expected = List(
      new ExpSymbol("("),
      new ExpSymbol("("),
      new ExpSymbol("foo"),
      new ExpInt(0),
      new ExpInt(1),
      new ExpSymbol(")"),
      new ExpSymbol(")"))
    Assert.assertEquals(expected, actual)
  }

}
