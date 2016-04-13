package engine

import scala.collection.SortedMap

/**
  * Created by FM on 08.04.16.
  */
object FooEngine extends EvaluationEngine {
  var inputStream = SortedMap.empty[Time, Set[EngineAtom]](
    Ordering.fromLessThan((l, r) => l.milliseconds < r.milliseconds)
  )

  override def append(time: Time)(atoms: Atom*): Unit = {
    inputStream = inputStream.updated(time, inputStream.getOrElse(time, Set[Atom]()) ++ atoms.toSet)
  }

  override def evaluate(time: Time): Set[Atom] = {
    inputStream.filterKeys(_.milliseconds <= time.milliseconds)
      .values
      .reduceOption((v, atoms) => v union atoms)
      .getOrElse(Set())
  }
}
