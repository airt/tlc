package io.airt.interpreter

object Library {

  def apply(s: String): Exp = lib(s)

  val lib: Map[String, Exp] = Map(
    "t" -> Exp(1),
    "nil" -> ExpNil,
    "car" -> Exp(_.head.head),
    "cdr" -> Exp(_.head.tail),
    "+" -> Exp(_ reduce (_ + _)),
    "-" -> Exp(_ reduce (_ - _)),
    "*" -> Exp(_ reduce (_ * _)),
    "/" -> Exp(_ reduce (_ / _)),
    "print" -> Exp { x =>
      x foreach println
      ExpNil
    }
  )

}
