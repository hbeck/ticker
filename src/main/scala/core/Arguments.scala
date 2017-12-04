package core

import core.Argument.Offset
import core.lars.TimePoint

/**
  * Created by FM on 15.06.16.
  */
trait Argument {

  override def hashCode(): Int = cachedHash

  lazy val cachedString = this.toString

  lazy val cachedHash = cachedString.hashCode

  override def equals(other: Any): Boolean = other match {
    case a: Argument => this.cachedHash == a.cachedHash
    case _ => false
  }

  def ==(other: Argument): Boolean = this.cachedHash == other.cachedHash

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

  implicit def convertToValue(intValue: IntValue): Value = intValue
}

trait NumericArgument extends Argument {
  def -(offset: Offset): NumericArgument

  def +(offset: Offset): NumericArgument
}

object NumericArgument{
  implicit def convertToNumericArgument(nameOrValue: String): NumericArgument = {
    if (nameOrValue.head.isUpper)
      StringVariable(nameOrValue)
    else
      IntValue(Integer.parseInt(nameOrValue))
  }
  implicit def convertToNumericArgument(argument: Argument): NumericArgument = {
    argument match {
      case na:NumericArgument => na
      case _ => throw new RuntimeException("cannot convert to numeric argument: "+argument)
    }
  }
}

trait Variable extends NumericArgument {
  val name: String
}

trait ArgumentWithOffset extends NumericArgument {
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

case class StringVariable(name: String) extends Variable {
  override def toString: String = name

  override def -(offset: Offset): NumericArgument = toVariableWithOffset(-offset)

  override def +(offset: Offset): NumericArgument = toVariableWithOffset(offset)

  private def toVariableWithOffset(initialOffset: Offset) = if (initialOffset == 0) {
    this
  } else {
    VariableWithOffset(this, initialOffset)
  }
}

case class VariableWithOffset(variable: Variable, offset: Offset = 0) extends Variable with ArgumentWithOffset {

  override def -(offset: Offset): NumericArgument = VariableWithOffset(variable, this.offset - offset)

  override def +(offset: Offset): NumericArgument = VariableWithOffset(variable, this.offset + offset)

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

  private lazy val precomputedHash = value.toString.hashCode

  override def hashCode(): Int = precomputedHash

}

case class IntValue(int: Int) extends Value with NumericArgument {

  override def toString = int.toString

  override def -(offset: Offset): NumericArgument = IntValue(int - offset)

  override def +(offset: Offset): NumericArgument = IntValue(int + offset)

  private lazy val precomputedHash = ("" + int).hashCode

  override def hashCode(): Int = precomputedHash

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
