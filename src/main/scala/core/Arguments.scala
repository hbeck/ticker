package core

import core.lars.TimePoint

/**
  * Created by FM on 15.06.16.
  */
trait Argument

object Argument {
  implicit def convertToArgument(nameOrValue: String): Argument = {
    if (nameOrValue.head.isUpper)
      StringVariable(nameOrValue)
    else
      Value(nameOrValue)
  }

  implicit def convertToValue(timePoint: TimePoint): Value = TimeValue(timePoint)
}

trait Variable extends Argument {
  val name: String
}

case class StringVariable(name: String) extends Variable {
  override def toString = name
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
}

case class TimeValue(timePoint: TimePoint) extends Value {
  override def toString = timePoint.value.toString
}

case class IntValue(int: Int) extends Value {
  override def toString = "" + int
}

object IntValue {
  def apply(value: String): IntValue = IntValue(Integer.parseInt(value))
}

object Value {
  def apply(timePoint: TimePoint): Value = TimeValue(timePoint)

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
