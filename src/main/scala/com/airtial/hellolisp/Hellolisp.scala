package com.airtial.hellolisp

object Hellolisp {

  def main(args: Array[String]) {

    println("Hellolisp.scala")

    if (args.length == 0) return

    val source = scala.io.Source.fromFile(args.head)

    val input: String = source.getLines()
      .foldLeft(new StringBuilder)(_.append(_))
      .toString()

    val result = eval(input)

    println("Result> " + result)

  }

  def eval(input: String): Exp = {
    Interpreter.interpret(Parser.parse(input))
  }

}
