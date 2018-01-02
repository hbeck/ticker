package runner

import java.util.concurrent.TimeUnit

import core.Atom
import core.lars.{ClockTime, TimePoint}
import engine._

import scala.concurrent.duration._

/**
  * Created by fm on 19/07/2017.
  */
sealed trait OutputWriting

sealed trait OutputWritingEvery[TUpdate] extends OutputWriting {
  def registerUpdate(update: TUpdate): Unit
  def shouldWriteAfterUpdate(update: TUpdate): Boolean
}

case class SignalBasedWriting(interval: Int = 1) extends OutputWritingEvery[Seq[Atom]] {

  private var lastUpdate: Int = 0

  def shouldWriteAfterUpdate(signals: Seq[Atom]): Boolean = lastUpdate + signals.size >= interval

  def registerUpdate(signals: Seq[Atom]) {
    lastUpdate = (lastUpdate + signals.size) % interval
  }
}

case class TimeBasedWriting(interval: Duration = 1 second, clockTime: ClockTime) extends OutputWritingEvery[TimePoint] {

  private var lastUpdateAt: Duration = 0 seconds

  def shouldWriteAfterUpdate(timepoint: TimePoint): Boolean = convertToDuration(timepoint) - lastUpdateAt >= interval

  def registerUpdate(timepoint: TimePoint): Unit = {
    if (shouldWriteAfterUpdate(timepoint)) {
      lastUpdateAt = convertToDuration(timepoint)
    }
  }

  private def convertToDuration(timePoint: TimePoint) = Duration(timePoint.value * clockTime.toMillis, TimeUnit.MILLISECONDS)

}

object ChangeBasedWriting extends OutputWritingEvery[Result] {

  private var lastResult: Result = NoResult

  def shouldWriteAfterUpdate(newResult: Result): Boolean = !newResult.equals(lastResult)

  def registerUpdate(newResult: Result) {
    lastResult = newResult
  }

}