package engine.asp.tms

import core._
import core.lars.TimePoint
import engine.asp.{MappedProgram, PinnedModel, PinnedStream}
import engine.asp.evaluation._
import engine.asp.tms.policies.TmsPolicy

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluation(pinnedAspProgram: MappedProgram, tmsPolicy: TmsPolicy) extends StreamingAspInterpreter {
  val incrementalProgram = PinnedAspToIncrementalAsp(pinnedAspProgram)

  val (groundRules, nonGroundRules) = incrementalProgram.rules partition (_.isGround)

  tmsPolicy.initialize(groundRules.map(x => GroundedNormalRule(x)))

  //TODO time and set of atoms need to be the input here, not a stream
  def apply(timePoint: TimePoint, pinnedStream: PinnedStream): Option[PinnedModel] = {
    val pin = Pin(timePoint)

    val groundedRules = pin.ground(nonGroundRules)
    val groundedStream = pin.ground(pinnedStream) //TODO pinnedStream map (_.toNormal) oder so

    tmsPolicy.add(timePoint)(groundedRules ++ groundedStream)

    val resultingModel = tmsPolicy.getModel(timePoint) match {
      case Some(model) => Some(asPinnedAtoms(model, timePoint))
      case None => None
    }

    tmsPolicy.remove(timePoint)(groundedRules ++ groundedStream)

    resultingModel
  }

  def asPinnedAtoms(model: Model, timePoint: TimePoint) = model map {
    case p: PinnedAtom => p
    case GroundAtom(p: PinnedAtom, Seq()) => p
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)

    //    case a: Atom => throw new IllegalArgumentException(f"The atom $a is an invalid result (it cannot be converted into a PinnedAtom)")
  }

}
