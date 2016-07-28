package engine.asp.oneshot

import clingo.ClingoConversion
import core.asp.NormalProgram
import core.lars.TimePoint
import engine.asp.{PinnedModel, PinnedStream}


trait StreamingAspInterpreter {
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
