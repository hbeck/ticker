package core.lars

/**
  * Created by FM on 05.04.16.
  */
trait Time {
  def -(duration: Duration): Time

  def +(duration: Duration): Time
}

object Time {
  implicit def convertToTimePoint(timePoint: Long): Time = TimePoint(timePoint)

  implicit def convertToTimePoint(timePoint: Int): Time = TimePoint(timePoint)
}

case class TimePoint(timePoint: Long) extends Time {
  def -(duration: Duration) = TimePoint(timePoint - duration)

  def +(duration: Duration) = TimePoint(timePoint + duration)

  override def toString = {
    timePoint.toString
  }
}

case class TimeVariable(variable: String, offset: Duration = 0) extends Time {

  // TODO: should ground be part of the Time-trait?
  def ground(timePoint: TimePoint) = TimePoint(timePoint.timePoint + offset)

  def +(duration: Duration) = TimeVariable(variable, duration)

  def -(duration: Duration) = TimeVariable(variable, -duration)

  override def toString: String = {
    // TODO: use match?

    if (offset < 0)
      return f"$variable - ${math.abs(offset)}"
    if (offset > 0)
      return f"$variable + ${math.abs(offset)}"

    variable
  }
}

object TimePoint {
  implicit val ordering = Ordering.by((time: TimePoint) => time.timePoint)

  implicit def convertToTimePoint(timePoint: Long): TimePoint = TimePoint(timePoint)

  implicit def convertToTimePoint(timePoint: Int): TimePoint = TimePoint(timePoint)
}

object AtTime {
  def second(seconds: Long) = Second(seconds)

  def minute(minutes: Long) = Minute(minutes)
}

object Second {
  def apply(seconds: Long) = TimePoint(seconds * 1000)
}

object Minute {
  def apply(minutes: Long) = Second(minutes * 60)
}
