package engine

import scala.collection.SortedMap

/**
  * Created by FM on 08.04.16.
  */
object Engine extends EvaluationEngine {
  var items = SortedMap.empty[Time, Set[EngineAtom]](
    Ordering.fromLessThan((l, r) => l.milliseconds < r.milliseconds)
  )
  //withDefaultValue Set[Atom]()

  override def append(time: Time)(atoms: Atom*): Unit = {
    items = items.updated(time, items.getOrElse(time, Set[Atom]()) ++ atoms.toSet)
  }

  override def evaluate(time: Time): Set[Atom] = {
    items.filterKeys(_.milliseconds <= time.milliseconds)
      .values
      .reduceOption((v, atoms) => v union atoms)
      .getOrElse(Set())
  }
}
