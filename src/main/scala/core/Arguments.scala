package core

/**
  * Created by FM on 15.06.16.
  */
trait Argument

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
}

object Argument {
  implicit def convertToArgument(nameOrValue: String): Argument = {
    if (nameOrValue.head.isUpper)
      return Variable(nameOrValue)
    else
      return Value(nameOrValue)
  }
}