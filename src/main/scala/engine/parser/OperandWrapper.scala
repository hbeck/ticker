package engine.parser

/**
  * Created by et on 22.03.17.
  */
case class OperandWrapper() {

}

object OperandWrapper {
  def apply(num: Int): OperandWrapper = {OperandWrapper()}
  def apply(variable: Char): OperandWrapper = {OperandWrapper()}
}
