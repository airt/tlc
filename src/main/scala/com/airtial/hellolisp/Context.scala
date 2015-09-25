package com.airtial.hellolisp

class Context(var scope: Map[String, Exp]) {
  var parent: Context = null

  def this(scope: Map[String, Exp], parent: Context) {
    this(scope)
    this.parent = parent
  }

  def birth() = {
    new Context(Map(), this)
  }

  def get(id: String): Exp = {
    if (this.scope.contains(id)) {
      scope.get(id).get
    } else if (parent != null) {
      parent.get(id)
    } else {
      throw new scala.RuntimeException(
        String.format("undefined symbol `%s' in %s", id, this.toString))
    }
  }

  def getAsExpFunction(id: String): ExpFunction = {
    this.get(id) match {
      case ef: ExpFunction =>
        ef
      case _ =>
        throw new scala.RuntimeException(
          String.format("undefined function `%s' in %s", id, this))
    }
  }

  def put(id: String, value: Exp): Context = {
    scope = scope + Tuple2(id, value)
    this
  }

  override def toString: String = {
    if (this == Context.library) "Library" else scope.toString()
  }

}

object Context {

  val library = new Context(Library.lib)

}