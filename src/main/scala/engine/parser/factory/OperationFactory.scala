package engine.parser.factory

import core._
import engine.parser.wrapper.ArithOperationWrapper

/**
  * Created by et on 22.03.17.
  *
  * This class still needs lots of implementation!!!
  */
case class OperationFactory(left: ArithOperationWrapper, func: String, right: OperandFactory)
  extends BodyTrait {

  val operation = create(left,func,right)

  def create(left: ArithOperationWrapper, func: String, right: OperandFactory): RelationAtom = {
    val op1 = left.op1
    val op2 = left.op2
    val arith = left.arith

    func match {
      case "=" =>
        op2 match {
          case o if op2.isDefined => arith match {
            case a if arith.isDefined => getRelation(arith.get, op1.arg, op2.get.arg, right)
          }
          case None => Eq(op1.arg,right.arg)
        }
      case ">" => if(op2.isDefined) {??? /*TODO throw exception*/} else Gt(op1.arg,right.arg)
      case "<" => if(op2.isDefined) {??? /*TODO throw exception*/} else Lt(op1.arg,right.arg)
      case ">=" => if(op2.isDefined) {??? /*TODO throw exception*/} else Geq(op1.arg,right.arg)
      case "<=" => if(op2.isDefined) {??? /*TODO throw exception*/} else Leq(op1.arg,right.arg)
      case "!=" => if(op2.isDefined) {??? /*TODO throw exception*/} else Neq(op1.arg,right.arg)
    }
  }

  def getRelation(op: String, arg11: Argument, arg12: Argument, arg2: OperandFactory): RelationAtom = op match {
    case "+" => Plus(arg11,arg12,arg2.arg)
    case "*" => Times(arg11,arg12,arg2.arg)
    case _ => ??? //TODO implement :)
  }
}
