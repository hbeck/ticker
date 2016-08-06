package core

import core.lars.TimePoint

/**
  * Created by FM on 15.06.16.
  */
sealed trait Argument

object Argument {
  implicit def convertToArgument(nameOrValue: String): Argument = {
    if (nameOrValue.head.isUpper)
      Variable(nameOrValue)
    else
      StringValue(nameOrValue)
  }

  implicit def convertToValue(timePoint: TimePoint): Value = TimeValue(timePoint)
}

case class Variable(name: String) extends Argument

object Variable {
  implicit def convertToVariable(name: String): Variable = {
    if (!name.head.isUpper)
      throw new IllegalArgumentException("A variable must start with an upper-case char")

    Variable(name)
  }
}

sealed trait Value extends Argument

case class StringValue(value: String) extends Value {
  override def toString = value
}

case class TimeValue(timePoint: TimePoint) extends Value {
  override def toString = timePoint.value.toString
}

object Value {
  def apply(timePoint: TimePoint): Value = TimeValue(timePoint)

  def apply(value: String): Value = StringValue(value)

  implicit def convertToValue(value: String): Value = {
    if (value.head.isUpper)
      throw new IllegalArgumentException("A value must not start with an upper-case char")

    StringValue(value)
  }


}
