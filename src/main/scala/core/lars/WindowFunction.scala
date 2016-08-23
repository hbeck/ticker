package core.lars

/**
  * Created by FM on 01.05.16.
  */
sealed trait WindowFunction

object FluentWindow extends WindowFunction

sealed trait SlidingWindow extends WindowFunction {
  val windowSize: WindowSize
}

case class SlidingTimeWindow(windowSize: WindowSize) extends SlidingWindow

case class SlidingTupleWindow(windowSize: WindowSize) extends SlidingWindow