package engine.parser.factory

import core._
import engine.parser.InvalidSyntaxException
import engine.parser.wrapper.OperationWrapper

/**
  * Created by et on 22.03.17.
  *
  */
case class OperationFactory(left: OperationWrapper, func: String, right: OperandFactory)
  extends BodyTrait {

  val operation:RelationAtom = create(left,func,right)

  def create(left: OperationWrapper, func: String, right: OperandFactory): RelationAtom = {
    if (isTernaryRelation(left)) {
      if (func == "=") getTernaryRelation(left, right.arg)
      else throw new InvalidSyntaxException("Invalid relation. For arithmetic operations only '=' is a valid relation.")
    } else {
      getBinaryRelation(func, left.op1.arg, right.arg)
    }
  }

  def isTernaryRelation(left: OperationWrapper): Boolean = left.op2 match {
    case None => false
    case _ => true
  }

  def getTernaryRelation(arg1: OperationWrapper, arg2: Argument): TernaryNumericRelationAtom =
    arg1.arith.get match {
    case "+" => Plus(arg1.op1.arg,arg1.op2.get.arg,arg2)
    case "-" => Minus(arg1.op1.arg,arg1.op2.get.arg,arg2)
    case "*" => Times(arg1.op1.arg,arg1.op2.get.arg,arg2)
    case "/" => Divide(arg1.op1.arg,arg1.op2.get.arg,arg2)
    case "%" => Modulo(arg1.op1.arg,arg1.op2.get.arg,arg2)
    case "^" => Power(arg1.op1.arg,arg1.op2.get.arg,arg2)
  }

  def getBinaryRelation(func: String, arg: Argument, arg2: Argument): BinaryRelationAtom = func match {
    case "=" => Eq(arg,arg2)
    case ">" => Gt(arg,arg2)
    case "<" => Lt(arg,arg2)
    case ">=" => Geq(arg,arg2)
    case "<=" => Leq(arg,arg2)
    case "!=" => Neq(arg,arg2)
    case _ => throw new InvalidSyntaxException("Relation "+func+" is not in the set {=,>,<,>=,<=,!=}") //TODO throw exception
  }
}

object OperationFactory {
  def apply(left: OperandFactory, func: String, right: OperandFactory): OperationFactory = {
    OperationFactory(OperationWrapper(left,None,None),func,right)
  }
  def apply(left: OperandFactory, func: String, right: OperationWrapper): OperationFactory = {
    var notFunc = ""
    func match {
      case ">" => notFunc = "<"
      case "<" => notFunc = ">"
      case ">=" => notFunc = "<="
      case "<=" => notFunc = ">="
      case _ => return OperationFactory(right,func,left)
    }
    OperationFactory(right,notFunc,left)
  }
}
