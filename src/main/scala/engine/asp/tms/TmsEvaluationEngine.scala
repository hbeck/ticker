package engine.asp.tms

import core._
import core.asp.{AspFact, AspProgram, NormalRule}
import core.grounding.{GrounderInstance, StaticProgramInspection}
import core.lars._
import engine._
import engine.asp._
import engine.asp.tms.policies.TmsPolicy

/**
  * Created by FM on 18.05.16.
  *
  * //TODO hb: deprecated?
  */
@deprecated
case class TmsEvaluationEngine(larsProgramEncoding: LarsProgramEncoding, tmsPolicy: TmsPolicy) extends EvaluationEngine {

  val incrementalProgram = IncrementalAspPreparation(larsProgramEncoding)

  val (groundRules, nonGroundRules) = incrementalProgram.rules.toSet.partition(_.isGround)

  val cachedResults = scala.collection.mutable.HashMap[TimePoint, Result]()

  tmsPolicy.initialize(groundRules.toSeq)

  //book keeping for auxiliary atoms to handle window logic
  var tuplePositions: List[Atom] = List()

  val tracker = new SignalTracker(larsProgramEncoding.maximumTimeWindowSizeInTicks, larsProgramEncoding.maximumTupleWindowSize, DefaultTrackedSignal.apply)

  override def append(time: TimePoint)(atoms: Atom*): Unit = {
    cachedResults(time) = prepare(time, atoms)
    discardOutdatedAuxiliaryAtoms(time)
  }

  //private def asFact(t: DefaultTrackedSignal): Seq[NormalRule] = Seq(t.timePinned, t.countPinned, t.timeCountPinned).map(AspFact[Atom](_))
  private def asFact(t: DefaultTrackedSignal): Seq[NormalRule] = Seq(t.timePinned, t.timeCountPinned).map(AspFact[Atom](_))

  def prepare(time: TimePoint, signalAtoms: Seq[Atom]): Result = {

    val tracked = tracker.track(time, signalAtoms)

    val pinnedSignals = tracked.flatMap(asFact)

    val allHistoricalSignals: Set[NormalRule] = tracker.allTimePoints(time).flatMap(asFact).toSet
    val pin = Pin(Assignment(Map(TimePinVariable -> time, CountPinVariable -> IntValue(tracker.tupleCount.toInt))))

    // performs simple pinning-calculations (eg. T + 1)
    val groundTimeVariableCalculations = nonGroundRules map (r => pin.groundTickVariables(r))

    val groundHeadsAsFacts: Set[NormalRule] = groundTimeVariableCalculations filter (_.isGround) map (g => AspFact[Atom](g.head))

    val inspectWithAllSignals = StaticProgramInspection.forAsp(AspProgram((groundTimeVariableCalculations ++ allHistoricalSignals ++ groundHeadsAsFacts).toList))
    val grounder = GrounderInstance.forAsp(inspectWithAllSignals)

    val grounded = groundTimeVariableCalculations flatMap (grounder.ground(_)) toSeq

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
          return UnknownResult()
        } else {
          prepare(time, Seq()).get
        }
      }
    }

    resultingModel match {
      case Some(m) => Result(Some(m))
      case None => NoResult
    }
  }

  def asPinnedAtoms(model: Model, timePoint: TimePoint): PinnedModel = model map {
    case p: PinnedAtAtom => p
    case GroundAtomWithArguments(p: Predicate, Seq(t: TimePoint)) => GroundPinnedAtAtom(Atom(p), t)
    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => PinnedAtom(a, timePoint)
  }

  def discardOutdatedAuxiliaryAtoms(time: TimePoint) = {
    val maxWindowTicks = 100
    val atomsToRemove = tracker.discardOutdatedSignals(time)
    atomsToRemove foreach { atom => tmsPolicy.remove(atom.time)(asFact(atom)) }
  }
}
