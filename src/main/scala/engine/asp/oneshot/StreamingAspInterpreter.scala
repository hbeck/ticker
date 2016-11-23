package engine.asp.oneshot

import clingo.ClingoConversion
import core.asp.NormalProgram
import core.lars.TimePoint
import engine.asp.{PinnedModel, PinnedStream}


trait StreamingAspInterpreter {
  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel]
}


trait InterpretationMode

object Clingo extends InterpretationMode

object TMS extends InterpretationMode
