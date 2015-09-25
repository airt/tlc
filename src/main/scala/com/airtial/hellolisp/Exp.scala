package com.airtial.hellolisp

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

  def apply(i: Int): Exp = new ExpNumeric(i.toInt)

  def apply(f: Double): Exp =
    new ExpNumeric(f.floor.toInt, ((f - f.floor) * ExpNumeric.DecimalDigits).toInt)

  def apply(list: List[Exp]): Exp = new ExpList(list)

  def apply(function: (List[Exp]) => Exp): Exp = new ExpFunction(function)

  def categorize(input: String): Exp = {
    val RInt = """^(-?(?:[1-9]\d*|0))$""".r
    val RFloat = """^(-?(?:[1-9]\d*|0)\.[0-9]+)$""".r
    input match {
      case RInt(i) =>
        new ExpNumeric(i.toInt)
      case RFloat(f) =>
        val ns: Array[String] = f.split('.')
        var ni = ns(0).toInt
        var nd =
          (ns(1) +
            "0" * (ExpNumeric.DecimalDigits.toString.length - ns(1).length - 1))
            .toInt
        ni += nd / 1000
        nd = nd % 1000
        new ExpNumeric(ni, nd)
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

class ExpNumeric(val i: Int, val d: Int) extends Exp {

  def this(i: Int) = this(i, 0)

  def toExpString: ExpString = new ExpString(this.toString)

  override def +(other: Exp): ExpNumeric = this.combine(other)(_ + _)

  override def -(other: Exp): ExpNumeric = this.combine(other)(_ - _)

  override def *(other: Exp): ExpNumeric = this.combine(other)(_ * _)

  override def /(other: Exp): ExpNumeric = this.combine(other)(_ / _)

  def combine(other: Exp)
             (f: (Int, Int) => Int): ExpNumeric = {
    other match {
      case io: ExpNumeric =>
        var ni = f(this.i, io.i)
        var nd = f(this.d, io.d)
        ni += nd / ExpNumeric.DecimalDigits
        nd = nd % ExpNumeric.DecimalDigits
        new ExpNumeric(ni, nd)
      case _ =>
        throw new scala.RuntimeException(this + " can't be calculated as ExpNumeric")
    }
  }

  override def toString: String = {
    if (this.d == 0) {
      "%d".format(i)
    } else {
      "%d.%03d".format(i, d)
    }
  }

  override def hashCode(): Int = this.i.## & this.d.## >> 3

  override def equals(other: Any): Boolean = {
    other match {
      case that: ExpNumeric =>
        if (this eq that) {
          true
        } else {
          this.## == that.## && this.i == that.i && this.d == that.d
        }
      case _ => false
    }
  }

}

object ExpNumeric {

  val DecimalDigits = 1000

}

//class ExpInt(val value: Int) extends ExpNumeric {
//
//  def this(s: String) = this(s.toInt)
//
//  def toExpInt: ExpInt = this
//
//  def toExpFloat: ExpFloat = new ExpFloat(value.toDouble)
//
//  def toExpString: ExpString = new ExpString(value.toString)
//
//  def +(other: Exp): ExpNumeric = this.combine(other)(_ + _)
//
//  def -(other: Exp): ExpNumeric = this.combine(other)(_ - _)
//
//  def *(other: Exp): ExpNumeric = this.combine(other)(_ * _)
//
//  def /(other: Exp): ExpNumeric = this.combine(other)(_ / _)
//
//  def combine(other: Exp)
//             (f: (Int, Int) => Int): ExpInt ={
//    other match {
//      case io : ExpInt=>
//        new ExpInt(f(this.value, oi.value))
//      case fo: ExpFloat=>
//        fo.com
//    }
//  }
//
//  override def toString: String = this.value.toString
//
//  override def hashCode(): Int = this.value.##
//
//  override def equals(other: Any): Boolean = {
//    other match {
//      case that: ExpInt =>
//        if (this eq that) {
//          true
//        } else {
//          this.## == that.## && this.value == that.value
//        }
//      case _ => false
//    }
//  }
//
//}
//
//class ExpFloat(val value: Double) extends ExpNumeric {
//
//  def this(s: String) = this(s.toDouble)
//
//  def toExpInt: ExpInt = new ExpInt(this.value.toInt)
//
//  def toExpFloat: ExpFloat = this
//
//  def toExpString: ExpString = new ExpString(value.toString)
//
//  def +(other: ExpFloat): ExpFloat = this.combine(other)(_ + _)
//
//  def -(other: ExpFloat): ExpFloat = this.combine(other)(_ - _)
//
//  def *(other: ExpFloat): ExpFloat = this.combine(other)(_ * _)
//
//  def /(other: ExpFloat): ExpFloat = this.combine(other)(_ / _)
//
//  def combine(other: ExpFloat)
//             (f: (Double, Double) => Double): ExpFloat =
//    new ExpFloat(f(this.value, other.value))
//
//  override def toString: String = this.value.toString
//
//  override def hashCode(): Int = this.value.##
//
//  override def equals(other: Any): Boolean = {
//    other match {
//      case that: ExpFloat =>
//        if (this eq that) {
//          true
//        } else {
//          java.lang.Math.abs(this.value - that.value) < 0.000001
//        }
//      case _ => false
//    }
//  }
//
//}
