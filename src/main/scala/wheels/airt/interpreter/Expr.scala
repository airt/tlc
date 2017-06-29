package wheels.airt.interpreter

sealed trait Expr

object Expr {

  def list(es: Expr*) = ExprList(es.toList)

  def cat(input: String): Expr = {
    val RInt = """^([+-]?(?:[1-9]\d*|0))$""".r
    val RFloat = """^([+-]?(?:[1-9]\d*|0)\.[0-9]+)$""".r
    input match {
      case RInt(s) =>
        ExprNumeric(BigDecimal(s))
      case RFloat(s) =>
        ExprNumeric(BigDecimal(s))
      case s =>
        ExprIdentifier(s)
    }
  }

}

case object ExprNil extends Expr

case class ExprIdentifier(s: String) extends Expr

case class ExprFunction(f: Seq[Expr] => Expr) extends Expr {

  def apply(es: Seq[Expr]): Expr = f(es)

}

case class ExprString(s: String) extends Expr {

  override def toString: String = s"""ExprString("$s")"""

}

case class ExprNumeric(n: BigDecimal) extends Expr {

  def toExprString: ExprString = ExprString(n.toString)

  def +(rhs: ExprNumeric) = ExprNumeric(this.n + rhs.n)

  def -(rhs: ExprNumeric) = ExprNumeric(this.n - rhs.n)

  def *(rhs: ExprNumeric) = ExprNumeric(this.n * rhs.n)

  def /(rhs: ExprNumeric) = ExprNumeric(this.n / rhs.n)

  def %(rhs: ExprNumeric) = ExprNumeric(this.n % rhs.n)

}

case class ExprList(es: Seq[Expr]) extends Expr {

  def apply(index: Int): Expr = es(index)

  def head: Expr = es.head

  def tail = ExprList(es.tail)

  def length: Int = es.length

}
