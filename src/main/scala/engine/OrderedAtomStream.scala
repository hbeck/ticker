package engine

import core._
import core.lars.TimePoint

import scala.collection.SortedMap

/**
  * Created by FM on 08.04.16.
  */
class OrderedAtomStream {

  var inputStream = SortedMap.empty[TimePoint, Seq[Atom]](
    Ordering.fromLessThan((l, r) => l.value < r.value)
  )

  def append(time: TimePoint, atoms: Seq[Atom]): Unit = {
    val previousValue = inputStream.getOrElse(time, Seq[Atom]())
    inputStream = inputStream.updated(time, previousValue ++ atoms)
  }

  def evaluate(time: TimePoint): Seq[Atom] = inputStream.getOrElse(time, Seq())

  def evaluateUntil(time: TimePoint): Stream = inputStream.range(0, time + 1).map(x => StreamEntry(x._1, x._2.toSet)).toSet

  def evaluateUntil_(time: TimePoint) = inputStream.range(0, time + 1).
    map(e => e._2 map (a => (e._1, a))).
    flatten.
    zipWithIndex.
    map {
      case ((t, atom), index) => PinnedSignal(atom, t, index + 1)
    }.
    toSet

}

trait TrackedAtom {
  val groundAtom: GroundAtom
  val time: TimePoint
  val position: Long

  lazy val timePinned: PinnedAtAtom = PinnedAtom(groundAtom, time)
  lazy val countPinned: PinnedCntAtom = PinnedAtom.asCount(groundAtom, IntValue(position.toInt))
}

case class AtomTracking[TAtom <: TrackedAtom](maxTimeWindowSizeInTicks: Long, maxTupleWindowSize: Long, trackBuilder: (GroundAtom, TimePoint, Long) => TAtom) {

  var tupleCount: Long = 0
  var signalStream: SortedMap[TimePoint, Seq[TAtom]] = SortedMap.empty[TimePoint, Seq[TAtom]](
    Ordering.fromLessThan((l, r) => l.value < r.value)
  )

  def discardOutdatedAtoms(time: TimePoint): Seq[TAtom] = {

    val atomsToRemove = signalStream filterKeys (t => t.value < time.value - maxTimeWindowSizeInTicks)
    signalStream = signalStream -- atomsToRemove.keySet

    // TODO: explicit expire also for tuple-positions?
    atomsToRemove.
      flatMap(_._2).
      filter(_.position < tupleCount - maxTupleWindowSize).
      toSeq
  }

  def trackAtoms(time: TimePoint, atoms: Seq[Atom]): Seq[TAtom] = {

    val trackedAtoms = atoms.zipWithIndex map { case (atom, position) =>
      trackBuilder(
        GroundAtom.assertGround(atom),
        time.value,
        tupleCount + position + 1 //zip begins with 0, hence + 1

      )
    }

    tupleCount = tupleCount + trackedAtoms.size
    signalStream = signalStream.updated(time, trackedAtoms ++ signalStream.getOrElse(time, Seq()))

    trackedAtoms
  }

  def allTimePoints(time: TimePoint): Seq[TAtom] = signalStream.range(0, time + 1).
    flatMap(_._2).
    toSeq


}
