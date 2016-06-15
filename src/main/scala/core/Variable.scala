package core

/**
  * Created by FM on 15.06.16.
  */
case class Variable(name: String)

object Variable {
  implicit def convertToVariable(name: String): Variable = {
    if (!name.head.isUpper)
      throw new IllegalArgumentException("A variable must start with an upper-case char")

    Variable(name)
  }
}
