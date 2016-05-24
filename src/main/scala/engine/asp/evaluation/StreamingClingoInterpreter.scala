package engine.asp.evaluation

import clingo.{ClingoConversion, ClingoProgram, _}
import core.{Atom, AtomWithArguments, Model, PinnedAtom}
import core.lars.TimePoint

/**
  *
  * Created by FM on 22.04.16.
  */
case class StreamingClingoInterpreter(program: ClingoProgram, clingoEvaluation: ClingoEvaluation = ClingoEvaluation()) extends StreamingAspInterpeter {

  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel] = {

    val transformed = pinnedAtoms map (ClingoConversion(_))

    val aspResult = clingoEvaluation(program ++ transformed).headOption

    aspResult match {
      case Some(model) => Some(StreamingClingoInterpreter.asPinnedAtom(model, timePoint))
      case None => None
    }
  }
}

object StreamingClingoInterpreter {
  def asPinnedAtom(model: Model, timePoint: TimePoint) = model map {
    case aa: AtomWithArguments => convertToPinnedAtom(aa, timePoint)
    // TODO: this should not be possible?
    case a: Atom => PinnedAtom(a, timePoint)
  }

  val numberFormat = """\d+""".r

  def convertToPinnedAtom(atom: AtomWithArguments, timePoint: TimePoint): PinnedAtom = {
    // TODO: there should be a more elegant way...
    // should probably go to clingo-parser?

    val lastArgument = atom.arguments.last

    val converted = numberFormat.findFirstIn(lastArgument) match {
      case Some(number) => {
        val l = number.toLong

        val atomWithoutTime = atom.arguments.init match {
          case Nil => atom.atom
          case remainingArguments => AtomWithArguments(atom.atom, remainingArguments)
        }

        PinnedAtom(atomWithoutTime, l)
      }
      // TODO: what todo when non matching number?
      case None => atom(timePoint)
    }

    converted
  }
}
