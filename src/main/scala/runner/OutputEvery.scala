package runner

import java.util.concurrent.TimeUnit

import core.Atom
import core.lars.TimePoint
import engine._

import scala.concurrent.duration._

/**
  * Created by fm on 19/07/2017.
  */
sealed trait OutputEvery

sealed trait TrackOutputEvery[TUpdate] extends OutputEvery {
  def shouldUpdateWithNewData(data: TUpdate): Boolean

  def registerUpdate(data: TUpdate): Unit
}

case class Signal(interval: Int = 1) extends TrackOutputEvery[Seq[Atom]] {
  private var lastUpdate: Int = 0

  def shouldUpdateWithNewData(signals: Seq[Atom]): Boolean = lastUpdate + signals.size >= interval

  def registerUpdate(signals: Seq[Atom]): Unit = lastUpdate = (lastUpdate + signals.size) % interval
}

case class Time(interval: Duration = 1 second) extends TrackOutputEvery[TimePoint] {
  private var lastUpdateAt: Duration = 0 micro
  private var engineSpeed: Duration = 1 micro

  def shouldUpdateWithNewData(time: TimePoint): Boolean = convertToDuration(time) - lastUpdateAt >= interval

  def registerUpdate(signals: TimePoint): Unit = lastUpdateAt = convertToDuration(signals)

  def registerEngineSpeed(engineSpeed: Duration): Unit = this.engineSpeed = engineSpeed

  private def convertToDuration(timePoint: TimePoint) = Duration(timePoint.value * engineSpeed.toMillis, TimeUnit.MILLISECONDS)

}

object Diff extends TrackOutputEvery[Result] {
  private var lastResult: Result = NoResult

  def shouldUpdateWithNewData(newResult: Result): Boolean = !(newResult equals lastResult)

  def registerUpdate(newResult: Result): Unit = lastResult = newResult

}