package reasoner.asp.oneshot

import clingo.{ClingoConversion, ClingoProgram, _}
import core._
import core.lars.TimePoint
import reasoner.asp.{PinnedModel, PinnedStream}


/**
  *
  * Created by FM on 22.04.16.
  */
case class StreamingClingoInterpreter(program: ClingoProgram, clingoCall: ClingoCall = ClingoCall()) extends StreamingAspInterpreter {

  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel] = {

    val transformed = pinnedAtoms map (ClingoConversion(_))

    val aspResult: Option[Model] = clingoCall(PlainClingoProgram(program.rules ++ transformed)).headOption

    aspResult match {
      case Some(model) => Some(model)
      case None => None
    }
  }
}

object StreamingClingoInterpreter {

  def asPinnedAtom(model: Model, timePoint: TimePoint): PinnedModel = model map {
    case p: PinnedAtom => p
    case aa: AtomWithArguments => convertToPinnedAtom(aa, timePoint)
    case a: Atom => throw new IllegalArgumentException(f"Cannot convert '$a' into a PinnedAtom")
  }

  val numberFormat = """\d+""".r

  def convertToPinnedAtom(atom: AtomWithArguments, timePoint: TimePoint): Atom = atom.arguments.last match {
    case StringValue(v) => convertValue(atom, v)
    case IntValue(v) => convertValue(atom, v)
    case _ => throw new IllegalArgumentException("Can only handle values as last argument")
  }

  def convertValue(atom: AtomWithArguments, value: String): Atom = numberFormat.findFirstIn(value) match {
    case Some(number) => convertValue(atom, number.toLong)
    case None => throw new IllegalArgumentException(f"Cannot convert '$value' into a TimePoint for a PinnedAtom")
  }

  def convertValue(atom: AtomWithArguments, value: Long): Atom = {
    val atomWithoutTime = atom.arguments.init match {
      case Nil => PredicateAtom(atom.predicate)
      case remainingArguments => NonGroundAtom(atom.predicate, remainingArguments)
    }

    if (reasoner.asp.specialPinPredicates.contains(atom.predicate)) {
      atomWithoutTime.appendArguments(Seq(IntValue(value.toInt)))
    }
    else {
      PinnedAtom.asPinnedAtAtom(atomWithoutTime, value)
    }
  }

}
