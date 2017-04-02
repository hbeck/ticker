package engine.parser.factory

import core.{Argument, IntValue, Variable}

/**
  * Created by et on 22.03.17.
  */
case class OperandFactory(operand: Any) {

  val arg: Argument = create(operand)

  def create(operand: Any): Argument = operand match {
    case arg: Double => Variable(arg.toString)
    case arg: Char => IntValue(arg.toInt)
    case _ => ??? //TODO throw exception
  }
}
