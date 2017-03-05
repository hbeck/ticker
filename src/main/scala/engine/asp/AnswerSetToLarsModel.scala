package engine.asp

import core._
import core.lars.TimePoint

/**
  * Created by FM on 20.06.16.
  */
object PinnedModelToLarsModel {
  def apply(timePoint: TimePoint, model: PinnedModel): Model = {

    val filtered = model filter {
      case p: PinnedAtAtom if p.atom == now => false
      //      case ConcretePinnedAtom(atom, time) if !program.atAtoms.exists(_.atom == atom) => time == timePoint
      case GroundPinnedAtAtom(atom, time) => time == timePoint
      case _ => true
    }
    filtered
    //    filtered map PinnedAspToIncrementalAsp.unpin
  }
}