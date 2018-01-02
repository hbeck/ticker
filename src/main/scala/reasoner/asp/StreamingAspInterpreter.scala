package reasoner.asp

import core.lars.TimePoint
import reasoner.{PinnedModel, PinnedStream}


trait StreamingAspInterpreter {
  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel]
}