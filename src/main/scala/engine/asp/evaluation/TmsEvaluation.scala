package engine.asp.evaluation

import core._
import core.lars.TimePoint
import jtms.ExtendedJTMS

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluation(pinnedAspProgram: PinnedAspProgram) extends StreamingAspInterpeter {
  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel] = {

    val groundedProgram = GroundPinnedAsp(timePoint)(pinnedAspProgram, pinnedAtoms)

    val tms = ExtendedJTMS(groundedProgram)

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
