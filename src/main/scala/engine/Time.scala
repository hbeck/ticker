package engine

/**
  * Created by FM on 05.04.16.
  */
trait Time

case class TimePoint(timePoint: Long) extends Time {

  override def toString = {
    timePoint.toString
  }
}

case class TimeVariable(variable: String) extends Time {

  override def toString = {
    variable
  }
}

object TimePoint {
  implicit val ordering = Ordering.by((time: TimePoint) => time.timePoint)
}

object At {
  def second(seconds: Long) = Second(seconds)

  def minute(minutes: Long) = Minute(minutes)
}

object Second {
  def apply(seconds: Long) = TimePoint(seconds * 1000)
}

object Minute {
  def apply(minutes: Long) = Second(minutes * 60)
}
