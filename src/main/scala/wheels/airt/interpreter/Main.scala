package wheels.airt.interpreter

object Main {

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

  lazy val eval: String => Expr = AstInterpreter.interpret compose Parser.parse

}
