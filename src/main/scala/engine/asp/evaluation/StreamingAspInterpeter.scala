package engine.asp.evaluation

import clingo.ClingoConversion
import core.Model
import core.asp.PlainAspProgram
import core.lars.TimePoint


trait StreamingAspInterpeter {
  // TODO: pass timepoint as arguments to streaming interperter? needed for TMS
  def apply(timePoint: TimePoint, pinnedAtoms: Set[PinnedAspRule]): Option[Model]
}

//{
//
//  def prepare(pinnedAtoms: Set[PinnedAspRule]):
//}

object StreamingAspInterpeter {

  def select(program: PlainAspProgram, interpretationMode: InterpretationMode): StreamingAspInterpeter = interpretationMode match {
    case Clingo => StreamingClingoInterpreter(ClingoConversion(program))
  }
}

trait InterpretationMode

object Clingo extends InterpretationMode

object TMS extends InterpretationMode
