package engine

import clingo.ClingoProgramWithLars
import core._
import core.lars.TimePoint
import engine.asp.Tick

import scala.collection.SortedMap

/**
  * Created by FM on 08.04.16.
  */
//class OrderedAtomStream {
//
//  var inputStream = SortedMap.empty[TimePoint, Seq[Atom]](
//    Ordering.fromLessThan((l, r) => l.value < r.value)
//  )
//
//  def append(time: TimePoint, atoms: Seq[Atom]): Unit = {
//    val previousValue = inputStream.getOrElse(time, Seq[Atom]())
//    inputStream = inputStream.updated(time, previousValue ++ atoms)
//  }
//
//  def evaluate(time: TimePoint): Seq[Atom] = inputStream.getOrElse(time, Seq())
//
//  def evaluateUntil(time: TimePoint): Stream = inputStream.range(0, time + 1).map(x => StreamEntry(x._1, x._2.toSet)).toSet
//
//  def evaluateUntil_(time: TimePoint) = inputStream.range(0, time + 1).
//    map(e => e._2 map (a => (e._1, a))).
//    flatten.
//    zipWithIndex.
//    map {
//      case ((t, atom), index) => PinnedSignal(atom, t, index + 1)
//    }.
//    toSet
//
//}

trait TrackedSignal {
  val signal: Atom
  val time: TimePoint
  val count: Long
}

case class DefaultTrackedSignal(signal: Atom, time: TimePoint, count: Long) extends TrackedSignal {
  lazy val timePinned: PinnedAtAtom = PinnedAtom.asPinnedAtAtom(signal, time)
  lazy val timeCountPinned: PinnedAtCntAtom = PinnedAtom.asPinnedAtCntAtom(signal, time, IntValue(count.toInt))
}

object DefaultTrackedSignal {
  def apply(signal: Atom, tick: Tick): DefaultTrackedSignal = DefaultTrackedSignal(signal, TimePoint(tick.time), tick.count)
}

object SignalTracker {
  def apply(program: ClingoProgramWithLars): SignalTracker[DefaultTrackedSignal] = SignalTracker(program.maximumTimeWindowSizeInTicks, program.maximumTupleWindowSize, DefaultTrackedSignal.apply)
}

case class SignalTracker[TTrackedSignal <: TrackedSignal](maxTimeWindowSizeInTicks: Long, maxTupleWindowSize: Long, trackBuilder: (GroundAtom, TimePoint, Long) => TTrackedSignal) {

  var tupleCount: Long = 0
  var signalStream: SortedMap[TimePoint, Seq[TTrackedSignal]] = SortedMap.empty[TimePoint, Seq[TTrackedSignal]](
    Ordering.fromLessThan((l, r) => l.value < r.value)
  )

  def discardOutdatedSignals(time: TimePoint): Seq[TTrackedSignal] = {

    val signalsToRemove = signalStream.filterKeys(t => t.value < time.value - maxTimeWindowSizeInTicks).
      filter(_._2.forall(_.count < tupleCount - maxTupleWindowSize))

    signalStream = signalStream -- signalsToRemove.keySet

    signalsToRemove.
      flatMap(_._2).
      toSeq
  }

  def track(time: TimePoint, signals: Seq[Atom]): Seq[TTrackedSignal] = {

    val trackedSignals = signals.zipWithIndex map { case (signal, position) =>
      trackBuilder(
        GroundAtom.assertGround(signal),
        time.value,
        tupleCount + position + 1 //zip begins with 0, hence + 1
      )
    }

    tupleCount = tupleCount + trackedSignals.size
    signalStream = signalStream.updated(time, signalStream.getOrElse(time, Seq()) ++ trackedSignals)

    trackedSignals
  }

  def track(time: TimePoint, signal: Atom): TTrackedSignal = {
    tupleCount = tupleCount + 1

    val trackedSignal = trackBuilder(
      GroundAtom.assertGround(signal),
      time.value,
      tupleCount
    )

    signalStream = signalStream.updated(time, signalStream.getOrElse(time, Seq()) :+ trackedSignal)

    trackedSignal
  }

  def allTimePoints(time: TimePoint): Seq[TTrackedSignal] = {
    signalStream.range(0, time + 1).
      flatMap(_._2).
      toSeq
  }


}
