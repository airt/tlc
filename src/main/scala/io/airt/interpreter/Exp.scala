package io.airt.interpreter

abstract class Exp {

  def toBoolean: Boolean = true

  def count: Int = 1

  def apply(index: Int): Exp = ExpNil

  def head: Exp = ExpNil

  def tail: Exp = ExpNil

  def +(other: Exp): Exp = ExpNil

  def -(other: Exp): Exp = ExpNil

  def *(other: Exp): Exp = ExpNil

  def /(other: Exp): Exp = ExpNil

}

object Exp {

  def apply(input: String): Exp = categorize(input)

  def apply(i: Int): Exp = new ExpNumeric(i)

  def apply(f: Double): Exp = new ExpNumeric(f)

  def apply(list: List[Exp]): Exp = new ExpList(list)

  def apply(function: (List[Exp]) => Exp): Exp = new ExpFunction(function)

  def categorize(input: String): Exp = {
    val RInt = """^([+-]?(?:[1-9]\d*|0))$""".r
    val RFloat = """^([+-]?(?:[1-9]\d*|0)\.[0-9]+)$""".r
    input match {
      case RInt(i) =>
        new ExpNumeric(i)
      case RFloat(f) =>
        new ExpNumeric(f)
      case _ =>
        new ExpSymbol(input)
    }
  }

}

object ExpNil extends Exp {

  override def toBoolean: Boolean = false

  override def toString: String = "Nil"

}

class ExpFunction(val call: (List[Exp]) => Exp) extends Exp {

  def apply(list: List[Exp]): Exp = call(list)

  override def toString: String = call.toString()

}

class ExpList(val values: List[Exp]) extends Exp {

  override def apply(index: Int): Exp = values(index)

  override def head: Exp = values.head

  override def tail: ExpList = new ExpList(values.tail)

  override def count: Int = values.foldLeft(2)(_ + _.count)

  override def toString: String = values.toString()

  override def hashCode(): Int = values.##

  override def equals(other: Any): Boolean = {
    other match {
      case that: ExpList =>
        if (this eq that) {
          true
        } else {
          this.## == that.## &&
            this.values.length == that.values.length &&
            this.values.indices.
              forall(index => this.values(index) == that.values(index))
        }
      case _ => false
    }
  }

}

class ExpSymbol(val value: String) extends Exp {

  override def toString: String = value.toString

  override def hashCode(): Int = value.##

  override def equals(other: Any): Boolean = {
    other match {
      case that: ExpSymbol =>
        if (this eq that) {
          true
        } else {
          this.## == that.## && this.value == that.value
        }
      case _ => false
    }
  }

}

class ExpString(val value: String) extends Exp {

  override def toString: String = value

  override def hashCode(): Int = value.##

  override def equals(other: Any): Boolean = {
    other match {
      case that: ExpString =>
        if (this eq that) {
          true
        } else {
          this.## == that.## && this.value == that.value
        }
      case _ => false
    }
  }

}

class ExpNumeric(val value: BigDecimal) extends Exp {

  def this(s: String) = this(BigDecimal(s))

  def toExpString: ExpString = new ExpString(toString)

  override def +(other: Exp): ExpNumeric = this.combine(other)(_ + _)

  override def -(other: Exp): ExpNumeric = this.combine(other)(_ - _)

  override def *(other: Exp): ExpNumeric = this.combine(other)(_ * _)

  override def /(other: Exp): ExpNumeric = this.combine(other)(_ / _)

  def combine(other: Exp)
    (f: (BigDecimal, BigDecimal) => BigDecimal): ExpNumeric = {
    other match {
      case on: ExpNumeric =>
        new ExpNumeric(f(this.value, on.value))
      case _ =>
        throw new scala.RuntimeException(other + " can't be calculated as Numeric")
    }
  }

  override def toString: String = value.toString()

  override def hashCode(): Int = value.##

  override def equals(other: Any): Boolean = {
    other match {
      case that: ExpNumeric =>
        if (this eq that) {
          true
        } else {
          this.## == that.## && this.value == that.value
        }
      case _ => false
    }
  }

}
