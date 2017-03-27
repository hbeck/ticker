package engine.asp.tms

import core._
import core.asp.{AspFact, NormalFact, NormalRule}
import core.lars.{Assignment, GroundRule, LarsProgramInspection, TimePoint}
import engine.asp._
import engine.asp.tms.policies.TmsPolicy
import engine._

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluationEngine(pinnedAspProgram: LarsProgramEncoding, tmsPolicy: TmsPolicy) extends EvaluationEngine {

  val incrementalProgram = PinnedAspToIncrementalAsp(pinnedAspProgram)
  val convertToPinned = PinnedModelToLarsModel(pinnedAspProgram)

  val (groundRules, nonGroundRules) = incrementalProgram.rules.toSet.partition(_.isGround)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  tmsPolicy.initialize(groundRules.toSeq)

  val tracker = new AtomTracking(pinnedAspProgram.maximumTimeWindowSizeInTicks, pinnedAspProgram.maximumTupleWindowSize, DefaultTrackedAtom.apply)

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    cachedResults(time) = prepare(time, atoms)
    discardOutdatedAuxiliaryAtoms(time)
  }

  private def asFact(t: TrackedAtom): Seq[NormalRule] = Seq(t.timePinned, t.countPinned, t.timeCountPinned).map(AspFact[Atom](_))

  def prepare(time: TimePoint, signalAtoms: Seq[Atom]): Result = {
    val tracked = tracker.trackAtoms(time, signalAtoms)
    val pinnedSignals = tracked.flatMap(asFact)

    // TODO hb: seems crazy to always create the entire sequence from scratch instead of updating a data structure
    // (we have three iterations over all values instead of a single addition of the new atoms;
    //  maybe we should use a data structure that maintains signalStream and allHistoricalSignals?)
    val allHistoricalSignals: Set[NormalRule] = tracker.allTimePoints(time).flatMap(asFact).toSet
    val pin = Pin(Assignment(Map(core.lars.T -> time, core.lars.C -> IntValue(tracker.tupleCount.toInt))))

    // performs simple pinning-calculations (eg. T + 1)
    val groundTimeVariableCalculations = nonGroundRules map (r => pin.ground(r))

    val groundHeadsAsFacts: Set[NormalRule] = groundTimeVariableCalculations filter (_.isGround) map (g => AspFact[Atom](g.head))
    val grounder = new GroundRule[NormalRule, Atom, Atom]()
    val inspectWithAllSignals = LarsProgramInspection.from((groundTimeVariableCalculations ++ allHistoricalSignals ++ groundHeadsAsFacts).toSeq)
    // TODO: grounding fails here
    val grounded = groundTimeVariableCalculations flatMap grounder.ground(inspectWithAllSignals) toSeq

    val nowAtom = AspFact[Atom](now(time))
    val cntAtom = AspFact[Atom](cnt(tracker.tupleCount))

    val tickAtoms = Seq(nowAtom, cntAtom)

    val add = tmsPolicy.add(time) _

    // separating the calls ensures maximum on support for rules
    // facts first
    add(tickAtoms)
    add(pinnedSignals)
    // then rules
    add(grounded)

    val model = tmsPolicy.getModel(time)

    val remove = tmsPolicy.remove(time) _

    // rules first
    remove(grounded)
    // then facts
    // we never remove extensional atoms explicitly (the policy might do it)
    remove(tickAtoms)

    model
  }

  override def evaluate(time: TimePoint): Result = {

    val resultingModel = cachedResults.get(time) match {
      case Some(result) => result.get
      case None => {
        if (cachedResults.nonEmpty && time.value < cachedResults.keySet.max.value) {
          return UnknownResult
        } else {
          prepare(time, Seq()).get
        }
      }
    }

    // TODO: model-cleaning is still needed (remote e.g a_at(t), filter only for current timepoint, ..)
    resultingModel match {
      case Some(m) => Result(Some(m))
      case None => NoResult
    }
  }

  def discardOutdatedAuxiliaryAtoms(time: TimePoint) = {
    val atomsToRemove = tracker.discardOutdatedAtoms(time)

    atomsToRemove foreach { atom => tmsPolicy.remove(atom.time)(asFact(atom)) }
  }
}
