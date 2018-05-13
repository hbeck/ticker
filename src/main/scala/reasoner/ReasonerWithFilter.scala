package reasoner

import core.GroundPinnedAtAtom
import core.lars.TimePoint
import core._


/**
  * Created by fm on 05/06/2017.
  */

case class ReasonerWithFilter(reasoner: Reasoner, resultFilter: ResultFilter) extends Reasoner {

  override def append(time: TimePoint)(atoms: Atom*): Unit = reasoner.append(time)(atoms: _*)

  override def evaluate(time: TimePoint): Result = resultFilter.filter(time, reasoner.evaluate(time))
}

case class ResultFilter(restrictTo: Option[Set[Predicate]]) {

  val fixedAuxiliaryAtomPredicates = specialPinPredicates.toSet

  def filter(timePoint: TimePoint, result: Result): Result = {

    restrictTo match {
      case Some(set) =>
        result.get match {
          case Some(model) => {

            val withoutAuxiliary = model filterNot { a => fixedAuxiliaryAtomPredicates.contains(a.predicate) }

            val filteredAfterTime = withoutAuxiliary collect {
              case GroundPinnedAtAtom(atom, t) if t == timePoint => convertAtom(atom)
              case g: GroundAtom => g
            }

            val restrictedOnly = filteredAfterTime filter { a => set.contains(a.predicate) }

            Result(restrictedOnly)
          }
          case None => EmptyResult
        }
      case None => result
    }


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
