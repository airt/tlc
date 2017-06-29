package wheels.airt.interpreter

import scala.collection.GenTraversableOnce

class Context(var internal: Map[String, Expr], private var parent: Option[Context] = None) {

  def derive: Context = {
    new Context(Map(), Some(this))
  }

  def get(id: String): Expr = {
    if (this.internal.contains(id)) {
      internal(id)
    } else if (parent.nonEmpty) {
      parent.get.get(id)
    } else {
      throw new scala.RuntimeException(
        String.format("undefined symbol `%s' in %s", id, this.toString))
    }
  }

  def getAsExpFunction(id: String): ExprFunction = {
    this.get(id) match {
      case ef: ExprFunction =>
        ef
      case _ =>
        throw new scala.RuntimeException(
          String.format("undefined function `%s' in %s", id, this)
        )
    }
  }

  def put(id: String, value: Expr): Context = {
    internal += (id -> value)
    this
  }

  def merge(m: GenTraversableOnce[(String, Expr)]): Context = {
    internal ++= m
    this
  }

  override def toString: String = {
    if (this == Context.builtIn) "BuiltIn" else internal.toString
  }

}

object Context {

  val builtIn = new Context(BuiltIn)

}
