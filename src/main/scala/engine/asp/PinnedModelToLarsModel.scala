package engine.asp

import core.{PinnedAtom, _}
import core.lars.TimePoint
import engine.asp.tms.PinnedAspToIncrementalAsp

/**
  * Created by FM on 20.06.16.
  */
object PinnedModelToLarsModel {
  def apply(timePoint: TimePoint, model: PinnedModel): Model = {

    val filtered = model filter {
      case PinnedAtom(`now`, _) => false
      case PinnedAtom(atom, time) => time == timePoint
      case _ => true
    }
// TODO: use corerct unpint method
    val unpinned = filtered map PinnedAspToIncrementalAsp.unpin

    unpinned
  }
}
