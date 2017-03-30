package engine.parser.wrapper

import engine.parser.factory.{BodyTrait, OperandFactory}

/**
  * Created by et on 28.03.17.
  */
case class ArithOperationWrapper(op1: OperandFactory, arith: Option[String], op2: Option[OperandFactory])
  extends BodyTrait{

/*  val operation = create(op1,arith,op2)

  def create(op1: OperandFactory, arith: Option[String], op2: Option[OperandFactory]): Argument = {
    op2 match {
      case None => op1.arg
      case o2 => arith match {
        case None => ???
        case a => getRelation(a.get,op1.arg,o2)
      }
    }
  }

  def getRelation(op: String, arg: Argument, o2: Option[OperandFactory]): Argument = op match{
    case "+" => return Sum()
    case "*" =>
    case _ =>
  }*/
}
