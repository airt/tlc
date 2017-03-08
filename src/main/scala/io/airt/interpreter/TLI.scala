package io.airt.interpreter

object TLI {

  def main(args: Array[String]) {

    if (args.isEmpty) {
      println("No file name was specified")
      System.exit(-1)
    }

    val source = scala.io.Source.fromFile(args.head)

    val input: String = source.getLines.
      foldLeft(new StringBuilder)(_ append _).
      toString

    val result = eval(input)

    println(s"result> $result")

  }

  def eval(input: String): Exp = {
    Interpreter.interpret(Parser.parse(input))
  }

}
