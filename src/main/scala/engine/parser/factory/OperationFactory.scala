package engine.parser.factory

/**
  * Created by et on 22.03.17.
  */
case class OperationFactory(left: List[(String, OperandFactory)], func: String, right: List[(String, OperandFactory)]) extends BodyTrait {

  def eval(list: List[(String,OperandFactory)]): Double = {1.0}
}
