package engine.asp.evaluation

import core._
import core.lars.TimePoint
import engine.asp.{MappedProgram, PinnedAspToIncrementalAsp}
import jtms.ExtendedJtms

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluation(pinnedAspProgram: MappedProgram) extends StreamingAspInterpreter {
  val incrementalAsp = PinnedAspToIncrementalAsp(pinnedAspProgram)

  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[PinnedModel] = {

    val groundedProgram = GroundPinned(timePoint).groundIfNeeded(incrementalAsp, pinnedAtoms)

    //TODO add and remove instead of naive recalling
    val tms = ExtendedJtms(groundedProgram)

    tms.getModel() match {
      case Some(model) => Some(asPinnedAtoms(model, timePoint))
      case None => None
    }
  }

  def asPinnedAtoms(model: Model, timePoint: TimePoint) = model map {
    case p: PinnedAtom => p
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)
    //    case a: Atom => throw new IllegalArgumentException(f"The atom $a is an invalid result (it cannot be converted into a PinnedAtom)")
  }

}
