package engine.parser.factory

import core.{Argument, IntValue, Variable}

/**
  * Created by et on 22.03.17.
  */
case class OperandFactory private (num: Option[Double] = None, variable: Option[Char] = None) {

 val arg: Argument = create(num,variable)

  def create(num: Option[Double], variable: Option[Char]): Argument = num match {
    case None => Variable(variable.get.toString)
    case _ => IntValue(num.get.toInt)
  }
}

object OperandFactory {
  def apply(_num: Double): OperandFactory = new OperandFactory(num = Some(_num))
  def apply(_variable: Char): OperandFactory = new OperandFactory(variable = Some(_variable))
}
