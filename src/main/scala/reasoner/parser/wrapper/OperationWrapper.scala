package reasoner.parser.wrapper

import reasoner.parser.factories.{BodyTrait, ArgumentFactory}

/**
  * Created by et on 28.03.17.
  */
case class OperationWrapper(op1: ArgumentFactory, arith: Option[String], op2: Option[ArgumentFactory])
  extends BodyTrait
