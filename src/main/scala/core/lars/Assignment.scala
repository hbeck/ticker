package core.lars

import core._

/**
  * Created by hb on 8/21/16.
  */
case class Assignment(binding: Map[Variable, Value]) { //TODO crisis class due to inconsistent use of types
  def apply(arg: Argument): Option[Value] =
    arg match {
      case v: TimeVariableWithOffset => {
        v.offset match {
          case 0 => {
            val opt: Option[Value] = binding get v
            opt match {
              case None => {
                val stringVal = StringVariable(v.variable.name) //try also this - hack due to potential type mismatch
                binding get stringVal
              }
              case someValue => someValue
            }
          }
          case _ => {
            val lookupObj = TimeVariableWithOffset(v.variable, 0) //hack due to type mismatch of map
            val boundValue = binding(lookupObj)
            Some(v.calculate(boundValue))
          }
        }
      }
      case v: ArgumentWithOffset if binding.contains(v.variable) => binding get v.variable
      case v: Variable => binding get v
      case _ => None
    }
}