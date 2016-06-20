package engine.asp.tms

import core._
import core.lars.TimePoint
import engine.{EvaluationEngine, Result}
import engine.asp._
import engine.asp.oneshot.AspEvaluationEngine
import engine.asp.tms.policies.TmsPolicy

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluationEngine(pinnedAspProgram: MappedProgram, tmsPolicy: TmsPolicy) extends EvaluationEngine {
  val incrementalProgram = PinnedAspToIncrementalAsp(pinnedAspProgram)

  val (groundRules, nonGroundRules) = incrementalProgram.rules partition (_.isGround)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  tmsPolicy.initialize(groundRules.map(x => GroundedNormalRule(x)))


  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    cachedResults(time) = prepare(time, PinToTimePoint(time).atoms(atoms.toSet))
  }

  def prepare(time: TimePoint, pinnedStream: PinnedStream): Result = {
    val pin = Pin(time)

    val groundedRules = pin.ground(nonGroundRules)
    val groundedStream = pin.ground(pinnedStream)

    tmsPolicy.add(time)(groundedRules ++ groundedStream)
    val model = tmsPolicy.getModel(time)
    tmsPolicy.remove(time)(groundedRules)

    model
  }

  override def evaluate(time: TimePoint): Result = {
    val resultingModel = cachedResults.get(time) match {
      case Some(result) => result.get
      case None => prepare(time, Set()).get
    }

    new Result {
      override def get: Option[Model] = Some(PinnedModelToLarsModel(time, asPinnedAtoms(resultingModel.get, time)))
    }
  }

  def asPinnedAtoms(model: Model, timePoint: TimePoint) = model map {
    case p: PinnedAtom => p
    case GroundAtom(p: PinnedAtom, Seq()) => p
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)
  }
}
