package engine.parser.wrapper

/**
  * Created by et on 22.03.17.
  */
case class OperationWrapper(left: List[(String, OperandWrapper)], func: String, right: List[(String, OperandWrapper)]) extends BodyTrait {

  def eval(list: List[(String,OperandWrapper)]): Double = {1.0}
}
