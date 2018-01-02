package reasoner

import core.GroundPinnedAtAtom
import core.lars.TimePoint
import core._


/**
  * Created by fm on 05/06/2017.
  */

case class ReasonerWithFilter(reasoner: Reasoner, filter: ResultFilter) extends Reasoner {

  override def append(time: TimePoint)(atoms: Atom*): Unit = reasoner.append(time)(atoms: _*)

  override def evaluate(time: TimePoint): Result = filter.filter(time, reasoner.evaluate(time))
}

case class ResultFilter(restrictTo: Set[Atom]) {

  val restrictToPredicates = restrictTo.map(_.predicate)

  val fixedAuxiliaryAtomPredicates = specialPinPredicates.toSet

  def filter(timePoint: TimePoint, result: Result): Result = {
    result.get match {
      case Some(model) => {

        val withoutAuxiliary = model filterNot { a => fixedAuxiliaryAtomPredicates.contains(a.predicate) }

        val filteredAfterTime = withoutAuxiliary collect {
          case GroundPinnedAtAtom(atom, t) if t == timePoint => convertAtom(atom)
          case g: GroundAtom => g
        }

        val restrictedOnly = filteredAfterTime filter { a => restrictToPredicates.contains(a.predicate) }

        Result(restrictedOnly)
      }
      case None => EmptyResult
    }
  }

  def filterToPredicates(model: Model, predicates: Set[Predicate]) = {
    model filterNot { a => predicates.contains(a.predicate) }
  }

  private val TimeAtomPattern = "(.+)_at".r
  //private val CntAtomPattern = "(.+)_cnt".r
  private val TimeCntAtomPattern = "(.+)_at_cnt".r

  def convertAtom(atom: Atom): Atom = {
    val arguments = Atom.unapply(atom).getOrElse(Seq())

    val predicateName = atom.predicate.caption match {
      case TimeCntAtomPattern(predicate) => predicate
      case TimeAtomPattern(predicate) => predicate
      case _ => atom.predicate.caption
    }

    Atom(Predicate(predicateName), arguments)
  }
}
