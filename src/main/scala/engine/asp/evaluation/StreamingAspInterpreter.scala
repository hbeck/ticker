package engine.asp.evaluation

import clingo.ClingoConversion
import core.asp.NormalProgram
import core.lars.TimePoint


trait StreamingAspInterpreter {
  // TODO: pass timepoint as arguments to streaming interpreter? needed for TMS
  // decide  if we always should pin the data-stream first to a variable and then ground it
  // (wouldn't be needed for the clingo-case)
  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel]
}

object StreamingAspInterpreter {

  def select(program: NormalProgram, interpretationMode: InterpretationMode): StreamingAspInterpreter = interpretationMode match {
    case Clingo => StreamingClingoInterpreter(ClingoConversion(program))
  }
}

trait InterpretationMode

object Clingo extends InterpretationMode

object TMS extends InterpretationMode
