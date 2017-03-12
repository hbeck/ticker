package core

import core.Argument.Offset
import core.lars.TimePoint

/**
  * Created by FM on 15.06.16.
  */
trait Argument { //TODO offset at too generic position ~> IntVariable etc
  def -(offset: Offset): Argument

  def +(offset: Offset): Argument

  override def hashCode(): Int = cachedHash

  lazy val cachedHash = this.toString.hashCode

  override def equals(other: Any): Boolean = other match {
    case a: Argument => this.cachedHash == a.cachedHash
    case _ => false
  }

}


object Argument {
  type Offset = Int

  implicit def convertToArgument(nameOrValue: String): Argument = {
    if (nameOrValue.head.isUpper)
      StringVariable(nameOrValue)
    else
      Value(nameOrValue)
  }

  implicit def convertToValue(timePoint: TimePoint): Value = timePoint
}

trait Variable extends Argument {
  val name: String
}

trait ArgumentWithOffset extends Argument {
  val variable: Variable
  val offset: Offset

  val name: String = variable.name

  override def toString = offset match {
    case o if o < 0 => f"$name - ${math.abs(offset)}"
    case o if o > 0 => f"$name + ${math.abs(offset)}"
    case o if o == 0 => name
  }

  def calculate(baseValue: Value): Value
}

//TODO hb review why offset?
case class StringVariable(name: String) extends Variable {
  override def toString: String = name

  override def -(offset: Offset): Argument = toVariableWithOffset(-offset)

  override def +(offset: Offset): Argument = toVariableWithOffset(offset)

  private def toVariableWithOffset(initialOffset: Offset) = if (initialOffset == 0) {
    this
  } else {
    VariableWithOffset(this, initialOffset)
  }
}

case class VariableWithOffset(variable: Variable, offset: Offset = 0) extends Variable with ArgumentWithOffset {

  override def -(offset: Offset): Argument = VariableWithOffset(variable, this.offset - offset)

  override def +(offset: Offset): Argument = VariableWithOffset(variable, this.offset + offset)

  def calculate(baseValue: Value): Value = baseValue match {
    case IntValue(v) => IntValue(v + offset)
  }
}

object Variable {

  def apply(name: String): Variable = StringVariable(name)

  implicit def convertToVariable(name: String): Variable = {
    if (!name.head.isUpper)
      throw new IllegalArgumentException("A variable must start with an upper-case char")

    StringVariable(name)
  }
}

trait Value extends Argument

case class StringValue(value: String) extends Value {
  override def toString = value

  override def -(offset: Offset): Argument = this

  override def +(offset: Offset): Argument = this

  private lazy val precomputedHash = value.toString.hashCode

  override def hashCode(): Int = precomputedHash

  /*
  override def equals(other: Any) = other match {
    case StringValue(s) => this.value == s
    case IntValue(i) => this.value == ""+i
    case TimePoint(i) => this.value == ""+i
    case _ => false
  }
  */


}

case class IntValue(int: Int) extends Value {

  override def toString = "" + int

  override def -(offset: Offset): Argument = IntValue(int - offset)

  override def +(offset: Offset): Argument = IntValue(int + offset)

  private lazy val precomputedHash = (""+int).hashCode

  override def hashCode(): Int = precomputedHash

  /*
  override def equals(other: Any) = other match {
    case IntValue(i) => this.int == i
    case StringValue(s) => ""+this.int == s
    case TimePoint(i) => this.int == i
    case _ => false
  }
  */

}

object IntValue {
  def apply(value: String): IntValue = IntValue(Integer.parseInt(value))
}

object Value {
  def apply(value: String): Value = if (value forall (_.isDigit)) {
    IntValue(Integer.parseInt(value))
  } else {
    StringValue(value)
  }

  def apply(value: Int): Value = IntValue(value)

  implicit def convertToValue(value: String): Value = {
    if (value.head.isUpper)
      throw new IllegalArgumentException("A value must not start with an upper-case char")

    StringValue(value)
  }


}
