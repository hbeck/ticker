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
      Value(nameOrValue)
  }
}

case class Variable(name: String) extends Argument

object Variable {
  implicit def convertToVariable(name: String): Variable = {
    if (!name.head.isUpper)
      throw new IllegalArgumentException("A variable must start with an upper-case char")

    Variable(name)
  }
}

case class Value(value: String) extends Argument

object Value {
  implicit def convertToValue(value: String): Value = {
    if (value.head.isUpper)
      throw new IllegalArgumentException("A value must not start with an upper-case char")

    Value(value)
  }

  implicit def convertToValue(timePoint: TimePoint): Value = Value(timePoint.toString)
}
