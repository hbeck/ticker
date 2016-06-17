package core.lars

/**
  * Created by FM on 05.04.16.
  */
sealed trait Time {
  def -(duration: Duration): Time

  def +(duration: Duration): Time
}

object Time {
  implicit def convertToTimePoint(timePoint: Long): Time = TimePoint(timePoint)

  implicit def convertToTimePoint(timePoint: Int): Time = TimePoint(timePoint)
}

case class TimePoint(value: Long) extends Time {
  def -(duration: Duration) = TimePoint(value - duration)

  def +(duration: Duration) = TimePoint(value + duration)

  override def toString = {
    value.toString
  }
}

object TimePoint {
  implicit val ordering = Ordering.by((time: TimePoint) => time.value)

  implicit def convertToTimePoint(timePoint: Long): TimePoint = TimePoint(timePoint)

  implicit def convertToTimePoint(timePoint: Int): TimePoint = TimePoint(timePoint)
}

case class TimeVariableWithOffset(variable: TimeVariable, offset: Duration = 0) extends Time {

  def ground(timePoint: TimePoint) = TimePoint(timePoint.value + offset)

  def +(duration: Duration) = TimeVariableWithOffset(variable, duration)

  def -(duration: Duration) = TimeVariableWithOffset(variable, -duration)

  override def toString: String = {
    val name = variable.name
    if (offset < 0)
      return f"$name - ${math.abs(offset)}"
    if (offset > 0)
      return f"$name + ${math.abs(offset)}"

    name
  }
}