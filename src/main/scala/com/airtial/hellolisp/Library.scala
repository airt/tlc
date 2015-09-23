package com.airtial.hellolisp

object Library {

  def apply(s: String) = lib(s)

  val lib: Map[String, Exp] = Map(
    "t" -> Exp(1),
    "nil" -> ExpNil,
    "car" -> Exp((x) => x.head.head),
    "cdr" -> Exp((x) => x.head.tail),
    "+" -> Exp((x) => x.reduce(_ + _)),
    "-" -> Exp((x) => x.reduce(_ - _)),
    "*" -> Exp((x) => x.reduce(_ * _)),
    "/" -> Exp((x) => x.reduce(_ / _)),
    "print" -> Exp((x) => {
      x.foreach(println(_))
      ExpNil
    })
  )

}
