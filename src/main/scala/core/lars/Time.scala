package core.lars

import core.Argument.Offset
import core._

/**
  * Created by FM on 05.04.16.
  */
sealed trait Time extends Argument

object Time {
  implicit def convertToTimePoint(timePoint: Long): Time = TimePoint(timePoint)

  implicit def convertToTimePoint(timePoint: Int): Time = TimePoint(timePoint)

  implicit def convertToTimeVariable(variable: Variable): Time = TimeVariableWithOffset(variable)
}

case class TimePoint(value: Long) extends Time with Value {

  def -(duration: Offset) = TimePoint(value - duration)

  def +(duration: Offset) = TimePoint(value + duration)

  override def toString = {
    value.toString
  }

  override def equals(other: Any) = other match {
    case v:Value => this.toString == v.toString
    case _ => false
  }

  private lazy val precomputedHash = (""+value).hashCode //uniformity with IntValue and StringValue

  override def hashCode(): Int = precomputedHash
}

object TimePoint {
  implicit val ordering = Ordering.by((time: TimePoint) => time.value)

  implicit def convertToTimePoint(timePoint: Long): TimePoint = TimePoint(timePoint)

  implicit def convertToTimePoint(timePoint: Int): TimePoint = TimePoint(timePoint)
}

case class TimeVariableWithOffset(variable: TimeVariable, offset: Offset = 0) extends Time with Variable with ArgumentWithOffset {

  override def calculate(baseValue: Value): TimePoint = baseValue match {
    case TimePoint(t) => TimePoint(t + offset)
  }

  override def +(duration: Offset) = TimeVariableWithOffset(variable, duration)

  override def -(duration: Offset) = TimeVariableWithOffset(variable, -duration)
}

object TimeVariableWithOffset {
  implicit def convertToTimeVariable(variable: Variable): TimeVariableWithOffset = TimeVariableWithOffset(variable)
}

case class IntVariableWithOffset(variable: Variable, offset: Offset = 0) extends Variable with ArgumentWithOffset {

  override def calculate(baseValue: Value): Value = baseValue match {
    case IntValue(t) => IntValue(t + offset)
  }

  override def +(duration: Offset) = IntVariableWithOffset(variable, duration)

  override def -(duration: Offset) = IntVariableWithOffset(variable, -duration)
}

object IntVariableWithOffset {
  implicit def convertToTimeVariable(variable: Variable): IntVariableWithOffset = IntVariableWithOffset(variable)
}

