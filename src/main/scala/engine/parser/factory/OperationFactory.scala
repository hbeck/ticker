package engine.parser.factory

import core._
import engine.parser.InvalidSyntaxException
import engine.parser.wrapper.OperationWrapper

/**
  * Created by et on 22.03.17.
  *
  */
case class OperationFactory(left: ArgumentFactory, func: String, right: OperationWrapper)
  extends BodyTrait {

  lazy val operation:RelationAtom = create(left,func,right)

  private def create(left: ArgumentFactory, func: String, right: OperationWrapper): RelationAtom = {
    if (isTernaryRelation(right)) {
      if (func == "=")
        if(!increment(right)) getTernaryRelation(left.arg, right)
        else getBinaryRelation("+",right.op1.arg,left.arg)
      else throw new InvalidSyntaxException("Invalid relation. For arithmetic operations only '=' is a valid relation.")
    } else {
      getBinaryRelation(func, left.arg, right.op1.arg)
    }
  }

  private def increment(right: OperationWrapper): Boolean = right.op2.get.arg match {
    case arg:IntValue if arg.int == 1 =>
      if(right.arith.get == "+") true
      else false
    case _ => false
  }

  private def isTernaryRelation(right: OperationWrapper): Boolean = right.op2 match {
    case None => false
    case _ => true
  }

  private def getTernaryRelation(arg1: Argument, arg2: OperationWrapper): TernaryNumericRelationAtom =
    arg2.arith.get match {
    case "+" => Plus(arg2.op1.arg,arg2.op2.get.arg,arg1)
    case "-" => Minus(arg2.op1.arg,arg2.op2.get.arg,arg1)
    case "*" => Times(arg2.op1.arg,arg2.op2.get.arg,arg1)
    case "/" => Divide(arg2.op1.arg,arg2.op2.get.arg,arg1)
    case "%" => Modulo(arg2.op1.arg,arg2.op2.get.arg,arg1)
    case "^" => Power(arg2.op1.arg,arg2.op2.get.arg,arg1)
  }

  private def getBinaryRelation(func: String, arg: Argument, arg2: Argument): BinaryRelationAtom = func match {
    case "=" => Eq(arg,arg2)
    case ">" => Gt(arg,arg2)
    case "<" => Lt(arg,arg2)
    case ">=" => Geq(arg,arg2)
    case "<=" => Leq(arg,arg2)
    case "!=" => Neq(arg,arg2)
    case "+" => Incr(arg,arg2)
    case _ => throw new InvalidSyntaxException("Relation "+func+" is not in the set {=,>,<,>=,<=,!=}") //TODO throw exception
  }
}

object OperationFactory {
  def apply(left: ArgumentFactory, func: String, right: ArgumentFactory): OperationFactory = {
    OperationFactory(left,func,OperationWrapper(right,None,None))
  }
  def apply(left: OperationWrapper, func: String, right: ArgumentFactory): OperationFactory = {
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
