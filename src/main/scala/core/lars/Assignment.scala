package core.lars

import core._

/**
  * Created by hb on 8/21/16.
  */
case class Assignment(binding: Map[Variable, Value]) {
  def apply(arg: Argument): Option[Value] =
    arg match {
      case v: ArgumentWithOffset if binding.contains(v.variable) => binding get v.variable
      case v: Variable => binding get v
      case _ => None
    }
}