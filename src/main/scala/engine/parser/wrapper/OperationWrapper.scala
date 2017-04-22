package engine.parser.wrapper

import engine.parser.factory.{BodyTrait, ArgumentFactory}

/**
  * Created by et on 28.03.17.
  */
case class OperationWrapper(op1: ArgumentFactory, arith: Option[String], op2: Option[ArgumentFactory])
  extends BodyTrait
