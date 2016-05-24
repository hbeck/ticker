package engine.asp.evaluation

import core._
import core.lars.TimePoint
import jtms.ExtendedJtms

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluation(pinnedAspProgram: PinnedProgram) extends StreamingAspInterpreter {
  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel] = {

    val groundedProgram = GroundPinned(timePoint)(pinnedAspProgram, pinnedAtoms)

    //TODO add and remove instead of naive recalling
    val tms = ExtendedJtms(groundedProgram)

    tms.getModel() match {
      case Some(model) => Some(asPinnedAtoms(model, timePoint))
      case None => None
    }
  }

  def asPinnedAtoms(model: Model, timePoint: TimePoint) = model map {
    case p: PinnedAtom => p
    // TODO: this shouldn't happen?
    case a: Atom => PinnedAtom(a, timePoint)
  }

}
