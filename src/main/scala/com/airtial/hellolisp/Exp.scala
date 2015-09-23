package com.airtial.hellolisp

abstract class Exp {

  def toBoolean: Boolean = true

  def count: Int = 1

  def apply(index: Int): Exp = ExpNil

  def head: Exp = ExpNil

  def tail: Exp = ExpNil

  def +(other: Exp): Exp = this.combine(other)(_ + _)

  def -(other: Exp): Exp = this.combine(other)(_ - _)

  def *(other: Exp): Exp = this.combine(other)(_ * _)

  def /(other: Exp): Exp = this.combine(other)(_ / _)

  def combine(other: Exp)
             (f: (ExpNumeric, ExpNumeric) => ExpNumeric): Exp = {
    this match {
      case exp: ExpNumeric if other.isInstanceOf[ExpNumeric] =>
        f(exp, other.asInstanceOf[ExpNumeric])
      case _ =>
        throw new scala.RuntimeException("Can't calc as ExpNumeric")
    }
  }

}

object Exp {

  def apply(input: String): Exp = categorize(input)

  def apply(i: Int): Exp = new ExpInt(i)

  def apply(f: Double): Exp = new ExpFloat(f)

  def apply(list: List[Exp]): Exp = new ExpList(list)

  def apply(function: (List[Exp]) => Exp): Exp = new ExpFunction(function)

  def categorize(input: String): Exp = {
    val RInt = """^(-?(?:[1-9]\d*|0))$""".r
    val RFloat = """^(-?(?:[1-9]\d*|0)\.[0-9]+)$""".r
    input match {
      case RInt(i) =>
        new ExpInt(i)
      case RFloat(f) =>
        new ExpFloat(f)
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

  def apply(list: List[Exp]) = call(list)

  override def toString: String = call.toString()

}

class ExpList(val values: List[Exp]) extends Exp {

  override def apply(index: Int): Exp = values(index)

  override def head: Exp = values.head

  override def tail: ExpList = new ExpList(values.tail)

  override def count: Int = values.foldLeft(2)(_ + _.count)

  override def toString: String = this.values.toString()

  override def hashCode(): Int = this.values.##

  override def equals(other: Any): Boolean = {
    other match {
      case that: ExpList =>
        if (this eq that) {
          true
        } else {
          this.## == that.## &&
            this.values.length == that.values.length &&
            this.values.indices
              .forall(index => this.values(index) == that.values(index))
        }
      case _ => false
    }
  }

}

class ExpSymbol(val value: String) extends Exp {

  override def toString: String = this.value.toString

  override def hashCode(): Int = this.value.##

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

  override def toString: String = this.value

  override def hashCode(): Int = this.value.##

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

abstract class ExpNumeric extends Exp {

  def toExpInt: ExpInt

  def toExpFloat: ExpFloat

  def toExpString: ExpString

  def +(other: ExpNumeric): ExpNumeric = this.combine(other)(_ + _)(_ + _)

  def -(other: ExpNumeric): ExpNumeric = this.combine(other)(_ - _)(_ - _)

  def *(other: ExpNumeric): ExpNumeric = this.combine(other)(_ * _)(_ * _)

  def /(other: ExpNumeric): ExpNumeric = this.combine(other)(_ / _)(_ / _)

  def combine(other: ExpNumeric)
             (fi: (ExpInt, ExpInt) => ExpNumeric)
             (ff: (ExpFloat, ExpFloat) => ExpNumeric): ExpNumeric = {
    this match {
      case e: ExpInt if other.isInstanceOf[ExpInt] =>
        fi(e, other.asInstanceOf[ExpInt])
      case _ =>
        ff(this.toExpFloat, other.toExpFloat)
    }
  }

}

class ExpInt(val value: Int) extends ExpNumeric {

  def this(s: String) = this(s.toInt)

  def toExpInt: ExpInt = this

  def toExpFloat: ExpFloat = new ExpFloat(value.toDouble)

  def toExpString: ExpString = new ExpString(value.toString)

  def +(other: ExpInt): ExpInt = this.combine(other)(_ + _)

  def -(other: ExpInt): ExpInt = this.combine(other)(_ - _)

  def *(other: ExpInt): ExpInt = this.combine(other)(_ * _)

  def /(other: ExpInt): ExpInt = this.combine(other)(_ / _)

  def combine(other: ExpInt)
             (f: (Int, Int) => Int): ExpInt =
    new ExpInt(f(this.value, other.value))

  override def toString: String = this.value.toString

  override def hashCode(): Int = this.value.##

  override def equals(other: Any): Boolean = {
    other match {
      case that: ExpInt =>
        if (this eq that) {
          true
        } else {
          this.## == that.## && this.value == that.value
        }
      case _ => false
    }
  }

}

class ExpFloat(val value: Double) extends ExpNumeric {

  def this(s: String) = this(s.toDouble)

  def toExpInt: ExpInt = new ExpInt(this.value.toInt)

  def toExpFloat: ExpFloat = this

  def toExpString: ExpString = new ExpString(value.toString)

  def +(other: ExpFloat): ExpFloat = this.combine(other)(_ + _)

  def -(other: ExpFloat): ExpFloat = this.combine(other)(_ - _)

  def *(other: ExpFloat): ExpFloat = this.combine(other)(_ * _)

  def /(other: ExpFloat): ExpFloat = this.combine(other)(_ / _)

  def combine(other: ExpFloat)
             (f: (Double, Double) => Double): ExpFloat =
    new ExpFloat(f(this.value, other.value))

  override def toString: String = this.value.toString

  override def hashCode(): Int = this.value.##

  override def equals(other: Any): Boolean = {
    other match {
      case that: ExpFloat =>
        if (this eq that) {
          true
        } else {
          java.lang.Math.abs(this.value - that.value) < 0.000001
        }
      case _ => false
    }
  }

}
