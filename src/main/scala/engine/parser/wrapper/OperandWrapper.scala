package engine.parser.wrapper

/**
  * Created by et on 22.03.17.
  */
case class OperandWrapper() {

}

object OperandWrapper {
  def apply(num: Double): OperandWrapper = {OperandWrapper()}
  def apply(variable: Char): OperandWrapper = {OperandWrapper()}
}
