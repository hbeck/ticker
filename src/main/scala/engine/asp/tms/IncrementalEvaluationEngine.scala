package engine.asp.tms

import core._
import core.asp.{AspFact, NormalRule}
import core.lars.{LarsProgramInspection, RuleGrounder, TimePoint}
import engine._
import engine.asp._
import engine.asp.tms.policies.TmsPolicy

import scala.collection.immutable.HashMap

/**
  * Created by FM, HB on Feb/Mar 2017.
  */
case class IncrementalEvaluationEngine(larsProgramEncoding: LarsProgramEncoding, tmsPolicy: TmsPolicy) extends EvaluationEngine {

  tmsPolicy.initialize(larsProgramEncoding.groundBaseRules)

  //time of the truth maintenance network due to previous append and result calls
  var networkTime: TimePoint = TimePoint(-1)

  override def append(time: TimePoint)(atoms: Atom*) {
    if (time.value < networkTime.value) {
      throw new RuntimeException("out-of-order events are not allowed. new signals for t=" + time + ", system time already at t'=" + networkTime)
    }
    updateTime(time)
    atoms foreach addToTms
  }

  override def evaluate(time: TimePoint): Result = {
    if (time.value < networkTime.value) {
      return new UnknownResult("cannot evaluate previous time t=" + time + ", system time already at t'=" + networkTime)
    }
    updateTime(time)
    tmsPolicy.getModel(time)
  }

  //
  //
  //

  val signalTracker = new SignalTracker(larsProgramEncoding.maximumTimeWindowSizeInTicks, larsProgramEncoding.maximumTupleWindowSize, DefaultTrackedSignal.apply)

  var rulesExpiringAtTime: Map[Long,Set[NormalRule]] = HashMap[Long,Set[NormalRule]]()
  var rulesExpiringAtCount: Map[Long,Set[NormalRule]] = HashMap[Long,Set[NormalRule]]()

  val incrementalRuleMaker = IncrementalRuleMaker(larsProgramEncoding)

  def updateTime(time: TimePoint) {
    if (time.value > networkTime.value) {
      for (t <- (networkTime.value + 1) to (time.value - 1)) {
        singleTimeIncrementTo(t, false)
      }
      singleTimeIncrementTo(networkTime.value, true)
    }

    discardOutdatedSignals(time)
  }

  //the option not to include those rules which expire immediately in a sequence of updates is an immediately available optimization
  //a more enhanced version would calculate the 'holes', i.e., the sequence of intermediate rules that will not be used (per window atom)
  //this is a more involved optimization going beyond a pure incremental approach (left for future work)
  def singleTimeIncrementTo(time: Long, includeImmediatelyExpiringRules: Boolean) {

    incrementalRuleMaker.rulesForTime(time,includeImmediatelyExpiringRules)

    //TODO

    networkTime = TimePoint(time)
  }

  def addToTms(signal: Atom) {
    signalTracker.trackSignal(networkTime, signal)
    //TODO



  }

  //
  //
  //

  //book keeping for auxiliary signals to handle window logic
  //var tuplePositions: List[Atom] = List()

  private def asFacts(t: DefaultTrackedSignal): Seq[NormalRule] = Seq(t.timePinned, t.countPinned, t.timeCountPinned).map(AspFact[Atom](_))

  def prepare(time: TimePoint, signalAtoms: Seq[Atom]): Result = {

    val previousTupleCount = signalTracker.tupleCount
    val trackedSignals = signalTracker.trackSignals(time, signalAtoms)
    val currentTupleCount = signalTracker.tupleCount

    val pinnedSignals = trackedSignals flatMap asFacts

    // TODO hb: updating instead of recomputing
    // (we have three iterations over all values instead of a single addition of the new signals;
    //  maybe we should use a data structure that maintains signalStream and entireStreamAsFacts?)
    val entireStreamAsFacts: Set[NormalRule] = signalTracker.allTimePoints(time).flatMap(asFacts).toSet

    val prevPosition = TickPosition(time,previousTupleCount) //TODO do we have to include time?
    val currPosition = TickPosition(time,currentTupleCount)

    val rulesToGround = larsProgramEncoding.rulesToGround(prevPosition,currPosition)

    //TODO incrementally update inspection
    val inspection = LarsProgramInspection.from(rulesToGround ++ entireStreamAsFacts)
    val preparedRuleGrounder = new RuleGrounder[NormalRule,Atom,Atom]().ground(inspection) _
    val groundedRulesToAdd = rulesToGround flatMap preparedRuleGrounder

    val add = tmsPolicy.add(time) _
    val remove = tmsPolicy.remove(time) _

    // separating the calls ensures maximum on support for rules
    // facts first
    add(pinnedSignals)
    add(groundedRulesToAdd)

    val outdatedRules = Set[NormalRule]()
    //remove(rulesToRemove)

    //TODO update managing structure

    tmsPolicy.getModel(time)
  }

  def asPinnedAtoms(model: Model, timePoint: TimePoint): PinnedModel = model map {
    case p: PinnedAtAtom => p
    case GroundAtomWithArguments(p: Predicate, Seq(t: TimePoint)) => GroundPinnedAtAtom(Atom(p), t)
    // in incremental mode we assume that all (resulting) signals are meant to be at T
    case a: Atom => PinnedAtom(a, timePoint)
  }

  def discardOutdatedSignals(time: TimePoint) = {
    //val maxWindowTicks = pinnedAspProgram.maximumWindowSize.ticks(pinnedAspProgram.tickSize)
    val maxWindowTicks = 100
    //TODO
    val signalsToRemove = signalTracker.discardOutdatedSignals(time)

    signalsToRemove foreach { atom => tmsPolicy.remove(atom.time)(asFacts(atom)) }
  }

  //private def trackSignals(time: TimePoint, signals: Seq[Atom]) = {
  //  tuplePositions = signals.toList ++ tuplePositions
  //    stream = stream.updated(time, signals.toSet ++ stream.getOrElse(time, Set()))
  //}
}
