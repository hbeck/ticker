package engine

import core.Atom
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
