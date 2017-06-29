package wheels.airt.interpreter

import scala.collection.immutable.DefaultMap

object BuiltIn extends DefaultMap[String, Expr] {

  private val internal = Map[String, Expr](
    "t" -> ExprNumeric(1),
    "nil" -> ExprNil,
    "car" -> ExprFunction(es => es.head),
    "cdr" -> ExprFunction(es => ExprList(es.tail)),
    "+" -> ExprFunction(_.map(_.asInstanceOf[ExprNumeric]).reduce(_ + _)),
    "-" -> ExprFunction(_.map(_.asInstanceOf[ExprNumeric]).reduce(_ - _)),
    "*" -> ExprFunction(_.map(_.asInstanceOf[ExprNumeric]).reduce(_ * _)),
    "/" -> ExprFunction(_.map(_.asInstanceOf[ExprNumeric]).reduce(_ / _)),
    "print" -> ExprFunction { es => es foreach println; ExprNil }
  )

  override def get(key: String): Option[Expr] = internal.get(key)

  override def iterator: Iterator[(String, Expr)] = internal.iterator

}
