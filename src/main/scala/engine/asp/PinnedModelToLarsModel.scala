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
      case p: PinnedAtom if p.atom == now => false
      case ConcretePinnedAtom(atom, time) => time == timePoint
      case _ => true
    }

    filtered map PinnedAspToIncrementalAsp.unpin
  }
}
