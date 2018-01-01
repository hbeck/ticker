package engine.asp.oneshot

import core.lars.TimePoint
import engine.asp.{PinnedModel, PinnedStream}


trait StreamingAspInterpreter {
  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel]
}