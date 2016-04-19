package engine

import scala.collection.SortedMap

/**
  * Created by FM on 08.04.16.
  */
class OrderedAtomStream  {
  var inputStream = SortedMap.empty[Time, Set[EngineAtom]](
    Ordering.fromLessThan((l, r) => l.milliseconds < r.milliseconds)
  )

  def append(time: Time)(atoms: Set[Atom]): Unit = {
    inputStream = inputStream.updated(time, inputStream.getOrElse(time, Set[Atom]()) ++ atoms)
  }

  def evaluate(time: Time): Set[Atom] = {
    inputStream.filterKeys(_.milliseconds <= time.milliseconds)
      .values
      .reduceOption((v, atoms) => v union atoms)
      .getOrElse(Set())
  }

}
