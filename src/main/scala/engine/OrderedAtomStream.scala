package engine

import core.Atom

import scala.collection.SortedMap

/**
  * Created by FM on 08.04.16.
  */
class OrderedAtomStream {
  var inputStream = SortedMap.empty[TimePoint, Set[Atom]](
    Ordering.fromLessThan((l, r) => l.timePoint < r.timePoint)
  )

  def append(time: TimePoint)(atoms: Set[Atom]): Unit = {
    val previousValue = inputStream.getOrElse(time, Set[Atom]())
    inputStream = inputStream.updated(time, previousValue ++ atoms)
  }

  def evaluate(time: TimePoint): Set[Atom] = {
    inputStream.getOrElse(time, Set())
  }

  def evaluateUntil(time: TimePoint): Stream = {
    val eval = inputStream.map(x => StreamEntry(x._1, x._2))
    eval.toSet
  }

}
