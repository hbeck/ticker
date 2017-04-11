package engine.parser.factory

import core.{Argument, IntValue, Variable}
import engine.parser.InvalidSyntaxException

/**
  * Created by et on 22.03.17.
  */
case class OperandFactory(operand: Any) {

  val arg: Argument = create(operand)

  def create(operand: Any): Argument = operand match {
    case arg: Double => IntValue(arg.##)
    case arg: Char => Variable(arg.toString)
    case _ => throw new InvalidSyntaxException("Invalid operand class: "+operand.getClass.toString) //TODO throw exception
  }
}
