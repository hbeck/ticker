package core.lars

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration


/**
  * Created by FM on 01.05.16.
  */
sealed trait WindowFunction

object FluentWindow extends WindowFunction

sealed trait SlidingWindow extends WindowFunction {
  //  val windowSize: TimeWindowSize
}

case class SlidingTimeWindow(windowSize: TimeWindowSize) extends SlidingWindow

case class SlidingTupleWindow(windowSize: TupleCount) extends SlidingWindow

case class SlidingSpecificTupleWindow(windowSize: TupleCount) extends SlidingWindow

case class TimeWindowSize(size: Long, unit: TimeUnit = TimeUnit.SECONDS) {
  def ticks(tickSize: EngineTick) = Duration(unit.toMillis(size) / tickSize.toMillis, tickSize.unit).length
}


object TimeWindowSize {
  implicit val ordering = Ordering.by((time: TimeWindowSize) => time.unit.toMillis(time.size))

  implicit def toWindowSize(size: Long): TimeWindowSize = TimeWindowSize(size)

  implicit def toWindowSize(size: Int): TimeWindowSize = TimeWindowSize(size)
}