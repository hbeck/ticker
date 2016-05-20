package engine.asp.evaluation

import core._
import core.lars.TimePoint
import jtms.ExtendedJTMS

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluation(pinnedAspProgram: PinnedAspProgram) extends StreamingAspInterpeter {
  def apply(timePoint: TimePoint, pinnedAtoms: Set[PinnedAspRule]): Option[Model] = {

    val groundedProgram = GroundPinnedAsp(timePoint)(pinnedAspProgram, pinnedAtoms)

    val tms = ExtendedJTMS(groundedProgram)

    tms.getModel()
  }
}
