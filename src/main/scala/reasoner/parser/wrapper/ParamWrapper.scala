package reasoner.parser.wrapper

import reasoner.parser.InvalidSyntaxException

/**
  * Created by et on 22.03.17.
  */
case class ParamWrapper(value: Double, unit: Option[String] = None) {

}

object ParamWrapper {
  def apply(value: String, unit: String): ParamWrapper = {
    ParamWrapper(valueToDouble(value),Some(unit))
  }

  def apply(value: String, unit: Option[String]): ParamWrapper = {
    ParamWrapper(valueToDouble(value),unit)
  }

  def apply(value: String): ParamWrapper = {
    ParamWrapper(valueToDouble(value))
  }

  @throws[InvalidSyntaxException]
  private def valueToDouble(value: String): Double = {
    if(value.forall {c => Character.isDigit(c) || c == '.'}) {
      return value.toDouble
    }
    throw new InvalidSyntaxException("The given value '"+value+"' is not a number.")
  }
}
