package wheels.airt.interpreter

import scala.collection.mutable.ListBuffer

object Parser {

  lazy val parse: String => Expr = {
    simplify compose parenthesize compose tokenize
  }

  lazy val simplify: Expr => Expr = {
    case list: ExprList if list.length == 1 => simplify(list.head)
    case expr => expr
  }

  lazy val tokenize: String => Seq[Expr] = { input =>
    val cs = input.split("\"")

    val codes =
      cs.indices.
        filter(_ % 2 == 0).
        map(cs).
        map(
          _.
            replaceAll( """\(""", " ( ").
            replaceAll( """\)""", " ) ").
            trim.
            split("\\s+").
            map(Expr.cat).
            toSeq
        )

    val strings =
      cs.
        indices.
        filter(_ % 2 == 1).
        map(cs).
        map(ExprString)

    val es: ListBuffer[Expr] =
      codes.zip(strings).foldLeft(ListBuffer[Expr]()) { (lb, le) =>
        val (other, string) = le
        lb ++= other
        lb += string
        lb
      }

    if (codes.length > strings.length) es ++= codes.last

    ExprIdentifier("(") +=: es += ExprIdentifier(")")

    es.toList
  }

  lazy val count: Expr => Int = {
    case ExprList(es) => es.map(count).sum + 2 // es.foldLeft(2)(_ + count(_))
    case _ => 1
  }

  def parenthesize(input: Seq[Expr]): Expr = {
    var es = input
    val values: ListBuffer[Expr] = ListBuffer()

    while (true) {
      if (es.isEmpty) {
        return ExprList(values.toList)
      } else {
        val t = es.head
        es = es.tail
        t match {
          case symbol: ExprIdentifier =>

            if (symbol.s == "(") {
              val ne = parenthesize(es)
              values += ne
              es = es.slice(count(ne) - 1, es.length - 1)

            } else if (symbol.s == ")") {
              return ExprList(values.toList)

            } else if (symbol.s == "'") {
              if (ExprIdentifier("(") == es.head) {
                val ne = Expr.list(ExprIdentifier("quote"), parenthesize(es.tail))
                values += ne
                es = es.slice(count(ne) - 3, es.length - 1)
              } else {
                throw new scala.RuntimeException(
                  String.format("unexpected %s after '", es.head))
              }
            } else {
              values += t
            }

          case _ =>
            values += t
        }
      }
    }
    ExprList(values.toList)
  }

}
