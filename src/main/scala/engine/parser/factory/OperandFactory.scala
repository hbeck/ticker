package engine.parser.factory

/**
  * Created by et on 22.03.17.
  */
case class OperandFactory() {

}

object OperandFactory {
  def apply(num: Double): OperandFactory = {OperandFactory()}
  def apply(variable: Char): OperandFactory = {OperandFactory()}
}
