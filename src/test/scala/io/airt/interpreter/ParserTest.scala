package io.airt.interpreter

import org.junit.{Assert, Test}

class ParserTest {

  @Test
  def testParse(): Unit = {
    val code = """(foo 0 (+ 1 (bar "aoe")))"""
    val actual = Parser.parse(code)
    val expected =
      new ExpList(List(new ExpSymbol("foo"), Exp(0),
        new ExpList(List(
          new ExpSymbol("+"), Exp(1),
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
          new ExpList(List(Exp(1), Exp(2), Exp(3)))
        )),
        new ExpList(List(new ExpSymbol("quote"),
          new ExpList(List(Exp(4), Exp(5), Exp(6)))
        )),
        new ExpList(List(new ExpSymbol("quote"),
          new ExpList(List(Exp(7), Exp(8), Exp(9)))
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
      new ExpNumeric(0),
      new ExpNumeric(1),
      new ExpSymbol(")"),
      new ExpSymbol(")"))
    Assert.assertEquals(expected, actual)
  }

}
