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
  *
  * (this class does the grounding, pinning is done by the IncrementalRuleMaker)
  */
case class IncrementalEvaluationEngine(larsProgramEncoding: LarsProgramEncoding, tmsPolicy: TmsPolicy) extends EvaluationEngine {

  tmsPolicy.initialize(larsProgramEncoding.groundBaseRules)

  //time of the truth maintenance network due to previous append and result calls
  var networkTime: TimePoint = TimePoint(-1)
  var tupleCount: Long = 0

  override def append(time: TimePoint)(atoms: Atom*) {
    if (time.value < networkTime.value) {
      throw new RuntimeException("out-of-order events are not allowed. new signals for t=" + time + ", system time already at t'=" + networkTime)
    }
    updateNetworkTimeTo(time)
    atoms foreach addSignalAtNetworkTime
  }

  override def evaluate(time: TimePoint): Result = {
    if (time.value < networkTime.value) {
      return new UnknownResult("cannot evaluate previous time t=" + time + ", system time already at t'=" + networkTime)
    }
    updateNetworkTimeTo(time)
    tmsPolicy.getModel(time)
  }

  //
  //
  //

  val signalTracker = new SignalTracker(larsProgramEncoding.maximumTimeWindowSizeInTicks, larsProgramEncoding.maximumTupleWindowSize, DefaultTrackedSignal.apply)

  val incrementalRuleMaker = IncrementalRuleMaker(larsProgramEncoding)

  def updateNetworkTimeTo(time: TimePoint) {
    if (time.value > networkTime.value) {
      for (t <- (networkTime.value + 1) to (time.value)) {
        singleTimeIncrementTo(t)
      }
    }
  }

  def singleTimeIncrementTo(time: Long) {

    networkTime = TimePoint(time)

    val incrementalRules: Seq[(Expiration, NormalRule)] = incrementalRuleMaker.allRulesToGroundAt(networkTime) //may contain ground rules
    val rulesToAdd = groundAndRegisterExpiration(incrementalRules)

    tmsPolicy.add(networkTime)(rulesToAdd)

    val rulesToRemove = expirationHandling.unregisterExpiredByNetworkTime()
    tmsPolicy.remove(networkTime)(rulesToRemove)

  }

  def groundAndRegisterExpiration(rules: Seq[(Expiration,NormalRule)]): Seq[NormalRule] = {

    val rulesToGround = rules map { case (_,r) => r }

    val entireStreamAsFacts: Set[NormalRule] = signalTracker.allTimePoints(networkTime).flatMap(asFacts).toSet //TODO incremental
    val inspection = LarsProgramInspection.from(rulesToGround ++ entireStreamAsFacts) //TODO incremental
    val ground = new RuleGrounder[NormalRule,Atom,Atom]().groundWith(inspection) _

    //expiration date of non-ground rule carries over to grounding (flat representation for grouping later)
    val groundRulesWithExpiration: Seq[(Expiration, NormalRule)]  = rules flatMap { case (expiration,rule) =>
      ground(rule) map (groundRule => (expiration,groundRule))
    }

    expirationHandling.register(groundRulesWithExpiration)

    groundRulesWithExpiration map { case (_,r) => r }
  }

  def addSignalAtNetworkTime(signal: Atom) {

    val trackedSignal = signalTracker.track(networkTime, signal)
    tupleCount = tupleCount + 1

    val signalFacts: Seq[(Expiration, NormalRule)] = incrementalRuleMaker.factsForSignal(trackedSignal)
    val incrementalRules: Seq[(Expiration, NormalRule)] = incrementalRuleMaker.allRulesToGroundForSignal(trackedSignal) //may contain ground rules

    expirationHandling.register(signalFacts)
    val rulesToAdd = groundAndRegisterExpiration(incrementalRules)

    tmsPolicy.add(networkTime)(signalFacts map { case (e,r) => r})
    tmsPolicy.add(networkTime)(rulesToAdd)

    val rulesToRemove = expirationHandling.unregisterExpiredByCount()
    tmsPolicy.remove(networkTime)(rulesToRemove)

  }

  object expirationHandling {

    var rulesExpiringAtTime: Map[Long,Set[NormalRule]] = HashMap[Long,Set[NormalRule]]()
    var rulesExpiringAtCount: Map[Long,Set[NormalRule]] = HashMap[Long,Set[NormalRule]]()

    def register(rules: Seq[(Expiration,NormalRule)]) = {
      val groupedByTime: Map[Long, Seq[(Long, NormalRule)]] = rules collect { case (e,r) if (e.time != -1) => (e.time,r) } groupBy { case (t,_) => t }
      val groupedByCount: Map[Long, Seq[(Long, NormalRule)]] = rules collect { case (e,r) if (e.count != -1) => (e.count,r) } groupBy { case (c,_) => c }
      groupedByTime foreach { case (t,seq) =>
        val rules = seq map { case (t,r) => r }
        rulesExpiringAtTime = rulesExpiringAtTime updated (t, rulesExpiringAtTime.getOrElse(t,Set()) ++ rules)
      }
      groupedByCount foreach { case (c,seq) =>
        val rules = seq map { case (c,r) => r }
        rulesExpiringAtCount = rulesExpiringAtTime updated (c, rulesExpiringAtCount.getOrElse(c,Set()) ++ rules)
      }
    }

    def unregisterExpiredByNetworkTime(): Seq[NormalRule] = {
      val time = networkTime.value
      if (!rulesExpiringAtTime.contains(time)) {
        return Seq()
      }
      val rules: Set[NormalRule] = rulesExpiringAtTime.get(time).get
      rulesExpiringAtTime = rulesExpiringAtTime - time
      rules.toSeq
    }

    def unregisterExpiredByCount(): Seq[NormalRule] = {
      if (!rulesExpiringAtCount.contains(tupleCount)) {
        return Seq()
      }
      val rules: Set[NormalRule] = rulesExpiringAtCount.get(tupleCount).get
      rulesExpiringAtCount = rulesExpiringAtCount - tupleCount
      rules.toSeq
    }
    
  }

  //
  //
  //

  //book keeping for auxiliary signals to handle window logic
  //var tuplePositions: List[Atom] = List()

  @deprecated
  def asFacts(t: DefaultTrackedSignal): Seq[NormalRule] = Seq(t.timePinned, t.countPinned, t.timeCountPinned).map(AspFact[Atom](_))



//  def asPinnedAtoms(model: Model, timePoint: TimePoint): PinnedModel = model map {
//    case p: PinnedAtAtom => p
//    case GroundAtomWithArguments(p: Predicate, Seq(t: TimePoint)) => GroundPinnedAtAtom(Atom(p), t)
//    // in incremental mode we assume that all (resulting) signals are meant to be at T
//    case a: Atom => PinnedAtom(a, timePoint)
//  }

  //private def trackSignals(time: TimePoint, signals: Seq[Atom]) = {
  //  tuplePositions = signals.toList ++ tuplePositions
  //    stream = stream.updated(time, signals.toSet ++ stream.getOrElse(time, Set()))
  //}
}
