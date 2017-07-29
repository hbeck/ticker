package runner

import java.util.concurrent.TimeUnit

import core.Atom
import core.lars.TimePoint
import engine._

import scala.concurrent.duration._

/**
  * Created by fm on 19/07/2017.
  */
sealed trait OutputTracking

sealed trait OutputTrackingEvery[TUpdate] extends OutputTracking {
  def registerUpdate(data: TUpdate): Unit

  def shouldUpdateWithNewData(data: TUpdate): Boolean
}

case class SignalTracking(interval: Int = 1) extends OutputTrackingEvery[Seq[Atom]] {

  private var lastUpdate: Int = 0

  def shouldUpdateWithNewData(signals: Seq[Atom]): Boolean = lastUpdate + signals.size >= interval

  def registerUpdate(signals: Seq[Atom]) = lastUpdate = (lastUpdate + signals.size) % interval
}

case class TimeTracking(interval: Duration = 1 second, engineSpeed: Duration) extends OutputTrackingEvery[TimePoint] {

  private var lastUpdateAt: Duration = 0 seconds

  def shouldUpdateWithNewData(time: TimePoint): Boolean = convertToDuration(time) - lastUpdateAt >= interval

  def registerUpdate(signals: TimePoint): Unit = {
    if (shouldUpdateWithNewData(signals))
      lastUpdateAt = convertToDuration(signals)
  }

  //  def registerEngineSpeed(engineSpeed: Duration): Unit = this.engineSpeed = engineSpeed

  private def convertToDuration(timePoint: TimePoint) = Duration(timePoint.value * engineSpeed.toMillis, TimeUnit.MILLISECONDS)

}

object DiffTracking extends OutputTrackingEvery[Result] {
  var lastResult: Result = NoResult

  def shouldUpdateWithNewData(newResult: Result): Boolean = !(newResult equals lastResult)

  def registerUpdate(newResult: Result) = lastResult = newResult

}