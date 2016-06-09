package engine.asp.evaluation

import core._
import core.asp._
import core.lars.TimePoint
import engine.asp.{MappedProgram, PinnedAspToIncrementalAsp}
import jtms.ExtendedJtms

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluation(pinnedAspProgram: MappedProgram, initialTms: ExtendedJtms = ExtendedJtms()) extends StreamingAspInterpreter {
  val incrementalProgram = PinnedAspToIncrementalAsp(pinnedAspProgram)
  val (fixedRules, incrementalRules) = findFixPoint(incrementalProgram)

  val tms = {
    val fixedProgram = AspProgram(fixedRules.map(x => GroundedNormalRule(x.head, x.pos, x.neg)).toList)

    fixedProgram.rules foreach initialTms.add

    initialTms
  }


  def apply(timePoint: TimePoint, pinnedStream: PinnedStream): Option[PinnedModel] = {
    val atTimePoint = GroundPinned(timePoint)

    val groundedRules = atTimePoint.groundIfNeeded(incrementalRules)
    val groundedStream = atTimePoint.groundIfNeeded(pinnedStream)

    groundedRules foreach tms.add
    groundedStream foreach tms.add

    val resultingModel = tms.getModel() match {
      case Some(model) => Some(asPinnedAtoms(model, timePoint))
      case None => None
    }

    groundedRules foreach tms.remove
    groundedStream foreach tms.remove

    resultingModel
  }

  def asPinnedAtoms(model: Model, timePoint: TimePoint) = model map {
    case p: PinnedAtom => p

    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)

    //    case a: Atom => throw new IllegalArgumentException(f"The atom $a is an invalid result (it cannot be converted into a PinnedAtom)")
  }

  def findFixPoint(normalProgram: NormalProgram) = {
    val g0 = GroundPinned(0).groundIfNeeded(normalProgram, Set())

    val fixedParts = normalProgram.rules.intersect(g0.rules)

    (fixedParts, normalProgram.rules.diff(fixedParts))
  }

}
