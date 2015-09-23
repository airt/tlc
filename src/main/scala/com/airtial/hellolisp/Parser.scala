package com.airtial.hellolisp

import scala.collection.mutable.ListBuffer

object Parser {

  def parse(input: String): Exp = {
    val exp: Exp = parenthesize(tokenize(input))
    exp match {
      case list: ExpList if list.values.length == 1 => list.head
      case _ => exp
    }
  }

  def tokenize(input: String): List[Exp] = {

    val cs: Array[String] = input.split("\"")

    val codes: List[List[Exp]] =
      cs.indices
        .filter(_ % 2 == 0)
        .map(cs(_))
        .map(
          _.replaceAll( """\(""", " ( ")
            .replaceAll( """\)""", " ) ")
            .trim
            .split("\\s+")
            .map(Exp.categorize)
            .toList)
        .toList

    val strings: List[Exp] =
      cs.indices
        .filter(_ % 2 == 1)
        .map(cs(_))
        .map(new ExpString(_))
        .toList

    val exps: ListBuffer[Exp] =
      codes.zip(strings) // zip => List[(List[Exp], Exp)]
        .foldLeft(ListBuffer(): ListBuffer[Exp])(
          (lb, le) => {
            val (other, string) = le
            lb ++= other
            lb += string
            lb
          })

    if (codes.length > strings.length)
      exps ++= codes.last
    exps.+=:(Exp("("))
    exps.+=(Exp(")"))
    exps.toList

  }

  def parenthesize(input: List[Exp]): Exp = {
    var exps: List[Exp] = input
    val values: ListBuffer[Exp] = ListBuffer()
    while (true) {
      if (exps.isEmpty) {
        return if (values.length == 1) {
          values.last
        } else {
          new ExpList(values.toList)
        }
      } else {
        val t = exps.head
        exps = exps.tail
        t match {
          case symbol: ExpSymbol =>

            if (symbol.value == "(") {
              val ne = parenthesize(exps)
              values += ne
              exps = exps.slice(ne.count - 1, exps.length - 1)

            } else if (symbol.value == ")") {
              return new ExpList(values.toList)

            } else if (symbol.value == "'") {
              if (Exp("(") == exps.head) {
                val ne = Exp(List(Exp("quote"), parenthesize(exps.tail)))
                values += ne
                exps = exps.slice(ne.count - 3, exps.length - 1)
              } else {
                throw new scala.RuntimeException(
                  String.format("unexpected %s after '", exps.head))
              }
            } else {
              values += t
            }

          case _ =>
            values += t
        }
      }
    }
    new ExpList(values.toList)
  }

}
