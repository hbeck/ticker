package engine.asp

import core.{PinnedAtom, _}
import core.lars.TimePoint

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

    val unpinned = filtered map (_.atom)

    unpinned
  }
}
