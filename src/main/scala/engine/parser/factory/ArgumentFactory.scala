package engine.parser.factory

import core.{Argument, IntValue, Variable}
import engine.parser.InvalidSyntaxException

/**
  * Created by et on 22.03.17.
  */
case class ArgumentFactory(operand: Any) {

  lazy val arg: Argument = create(operand)

  def create(operand: Any): Argument = operand match {
    case arg: Double  => IntValue(arg.##)
    case arg: Int     => IntValue(arg)
    case arg: String  => Variable(arg)
    case _            => throw new InvalidSyntaxException("Invalid operand class: "+operand.getClass.toString)
  }
}
