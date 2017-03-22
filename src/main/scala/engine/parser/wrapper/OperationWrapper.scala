package engine.parser.wrapper

import engine.parser.OperandWrapper

/**
  * Created by et on 22.03.17.
  */
case class OperationWrapper(left: List[(String, OperandWrapper)], func: String, right: List[(String, OperandWrapper)]) extends BodyTrait {

}
