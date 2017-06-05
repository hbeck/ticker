package engine

import core._
import core.lars.TimePoint

/**
  * Created by fm on 05/06/2017.
  */

case class EvaluationEngineWithResultFilter(evaluationEngine: EvaluationEngine, filter: AtomResultFilter) extends EvaluationEngine {

  override def append(time: TimePoint)(atoms: Atom*): Unit = evaluationEngine.append(time)(atoms: _*)

  override def evaluate(time: TimePoint): Result = filter.filter(time, evaluationEngine.evaluate(time))
}

case class AtomResultFilter(restrictTo: Set[Atom]) {

  val restrictToPredicates = restrictTo.map(_.predicate)

  val fixedAuxiliaryAtomPredicates = asp.specialPinPredicates toSet

  def filter(timePoint: TimePoint, result: Result) = {
    result.get match {
      case Some(model) => {

        val withoutAuxiliary = model filterNot { a => fixedAuxiliaryAtomPredicates.contains(a.predicate) }
        val restrictedOnly = withoutAuxiliary filter { a => restrictToPredicates.contains(a.predicate) }

        val filteredAfterTime = restrictedOnly collect {
          case GroundPinnedAtAtom(atom, t) if t == timePoint => atom
          case p: PredicateAtom => p
        }

        Result(filteredAfterTime)
      }
      case None => EmptyResult
    }
  }

  def filterToPredicates(model: Model, predicates: Set[Predicate]) = {
    model filterNot { a => predicates.contains(a.predicate) }
  }

}
