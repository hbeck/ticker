package engine.asp.tms

import core._
import core.asp.NormalRule
import core.grounding.incremental.IncrementalAspGrounder
import core.lars.TimePoint
import engine._
import engine.asp._
import engine.asp.tms.policies.TmsPolicy

import scala.collection.immutable.HashMap

/**
  * Created by FM, HB on Feb/Mar 2017.
  *
  * (This class coordinates pinning (within IncrementalRuleMaker) and (then) grounding (IncrementalGrounder))
  */
case class IncrementalEvaluationEngine(incrementalRuleMaker: IncrementalRuleMaker, tmsPolicy: TmsPolicy) extends EvaluationEngine {

  val grounder = IncrementalAspGrounder()
  grounder.add(incrementalRuleMaker.staticGroundRules)
  tmsPolicy.initialize(incrementalRuleMaker.staticGroundRules)

  //time of the truth maintenance network due to previous append and result calls
  var currentTick = TickPair(0,0) //using (-1,0), first + will fail!
  singleOneDimensionalTickIncrement() //...therefore, surpass the increment and generate groundings for (0,0)

  override def append(time: TimePoint)(atoms: Atom*) {
    if (time.value < currentTick.time) {
      throw new RuntimeException("cannot append signal at past time t=" + time + ". system time already at t'=" + currentTick.time)
    }
    updateTimeTo(time)
    atoms foreach addSignalAtCurrentTime
  }

  override def evaluate(time: TimePoint): Result = {
    if (time.value < currentTick.time) {
      return new UnknownResult("cannot evaluate past time t=" + time + ". system time already at t'=" + currentTick.time)
    }
    updateTimeTo(time)
    tmsPolicy.getModel(time)
  }

  //
  //
  //

  def updateTimeTo(time: TimePoint) {
    if (time.value > currentTick.time) {
      for (t <- (currentTick.time + 1) to (time.value)) {
        singleTimeIncrementTo(t)
      }
    }
  }

  def singleTimeIncrementTo(time: Long) {
    currentTick = currentTick.incrementTime()
    singleOneDimensionalTickIncrement()
  }

  def addSignalAtCurrentTime(signal: Atom) {
    currentTick = currentTick.incrementCount()
    singleOneDimensionalTickIncrement(Some(signal))
  }

  //method to be called whenever time xor count increases by 1
  def  singleOneDimensionalTickIncrement(signal: Option[Atom]=None) {
    val rulesToGround: Seq[(Expiration, NormalRule)] = incrementalRuleMaker.rulesToGroundFor(currentTick, signal)
    rulesToGround foreach { case (_,r) => grounder.add(r) }
    val rulesToAdd = rulesToGround flatMap { case (e,r) =>
      val rules = grounder.ground(r)
      if (!rules.isEmpty) expirationHandling.register(e,rules)
      rules
    }
    println("\nrules added to tms at "+currentTick)
    rulesToAdd foreach println
    tmsPolicy.add(currentTick.time)(rulesToAdd)
    val rulesToRemove = signal match { //logic somewhat implicit...
      case None => expirationHandling.unregisterExpiredByTime()
      case _ => expirationHandling.unregisterExpiredByCount()
    }
    grounder.remove(rulesToRemove)
    tmsPolicy.remove(currentTick.time)(rulesToRemove)
  }

  object expirationHandling {

    var rulesExpiringAtTime: Map[Long,Set[NormalRule]] = HashMap[Long,Set[NormalRule]]()
    var rulesExpiringAtCount: Map[Long,Set[NormalRule]] = HashMap[Long,Set[NormalRule]]()

    def register(expiration: Expiration, rules: Set[NormalRule]) {
      val t = expiration.time
      val c = expiration.count
      if (t != Void) {
        rulesExpiringAtTime = rulesExpiringAtTime updated(t, rulesExpiringAtTime.getOrElse(t, Set()) ++ rules)
      }
      if (c != Void) {
        rulesExpiringAtCount = rulesExpiringAtCount updated(c, rulesExpiringAtCount.getOrElse(c, Set()) ++ rules)
      }
    }

    def unregisterExpiredByTime(): Seq[NormalRule] = {
      if (!rulesExpiringAtTime.contains(currentTick.time)) {
        return Seq()
      }
      val rules: Set[NormalRule] = rulesExpiringAtTime.get(currentTick.time).get
      rulesExpiringAtTime = rulesExpiringAtTime - currentTick.time
      rules.toSeq
    }

    def unregisterExpiredByCount(): Seq[NormalRule] = {
      if (!rulesExpiringAtCount.contains(currentTick.count)) {
        return Seq()
      }
      val rules: Set[NormalRule] = rulesExpiringAtCount.get(currentTick.count).get
      rulesExpiringAtCount = rulesExpiringAtCount - currentTick.count
      rules.toSeq
    }
    
  }

  //
  //
  //

  //book keeping for auxiliary signals to handle window logic
  //var tuplePositions: List[Atom] = List()

  //@deprecated
  //def asFacts(t: DefaultTrackedSignal): Seq[NormalRule] = Seq(t.timePinned, t.countPinned, t.timeCountPinned).map(AspFact[Atom](_))

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
