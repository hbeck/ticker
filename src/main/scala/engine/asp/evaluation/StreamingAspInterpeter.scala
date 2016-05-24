package engine.asp.evaluation

import clingo.ClingoConversion
import core.Model
import core.asp.PlainAspProgram
import core.lars.TimePoint


trait StreamingAspInterpeter {
  // TODO: pass timepoint as arguments to streaming interperter? needed for TMS
  // decide  if we always should pin the data-stream first to a variable and then ground it
  // (wouldn't be needed for the clingo-case)
  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[Model]
}

object StreamingAspInterpeter {

  def select(program: PlainAspProgram, interpretationMode: InterpretationMode): StreamingAspInterpeter = interpretationMode match {
    case Clingo => StreamingClingoInterpreter(ClingoConversion(program))
  }
}

trait InterpretationMode

object Clingo extends InterpretationMode

object TMS extends InterpretationMode
