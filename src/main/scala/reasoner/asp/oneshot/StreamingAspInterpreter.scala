package reasoner.asp.oneshot

import core.lars.TimePoint
import reasoner.asp.{PinnedModel, PinnedStream}


trait StreamingAspInterpreter {
  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel]
}