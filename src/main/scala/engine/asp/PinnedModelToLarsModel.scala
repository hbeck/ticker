package engine.asp

import core.{PinnedAtom, _}
import core.lars.{LarsBasedProgram, TimePoint}
import engine.asp.tms.PinnedAspToIncrementalAsp

/**
  * Created by FM on 20.06.16.
  */
case class PinnedModelToLarsModel(program: LarsBasedProgram) {
  def apply(timePoint: TimePoint, model: PinnedModel): Model = {

    val filtered = model filter {
      case p: PinnedAtom if p.atom == now => false
//      case ConcretePinnedAtom(atom, time) if !program.atAtoms.exists(_.atom == atom) => time == timePoint
      case ConcretePinnedAtom(atom, time)  => time == timePoint
      case _ => true
    }

    filtered map PinnedAspToIncrementalAsp.unpin
  }
}