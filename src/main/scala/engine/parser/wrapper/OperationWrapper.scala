package engine.parser.wrapper

import engine.parser.factory.{BodyTrait, OperandFactory}

/**
  * Created by et on 28.03.17.
  */
case class OperationWrapper(op1: OperandFactory, arith: Option[String], op2: Option[OperandFactory])
  extends BodyTrait
