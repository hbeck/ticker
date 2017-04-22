package engine.asp.tms

import core._
import core.asp.NormalRule
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

  tmsPolicy.initialize(incrementalRuleMaker.staticGroundRules) //TODO 0422 include those of window rules that do not change

  //time of the truth maintenance network due to previous append and result calls
  var currentTick = Tick(0, 0) //using (-1,0), first "+" will fail!
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

  def singleOneDimensionalTickIncrement(signal: Option[Atom] = None) {

    val expiringRules: Seq[ExpiringNormalRule] = incrementalRuleMaker.rulesToAddFor(currentTick, signal)
    expiringRules foreach (expirationHandling.register(_))
    val rulesToAdd = expiringRules map (_.rule)

    if (IEEConfig.printRules) {
      println("rules added at tick " + currentTick)
      rulesToAdd foreach println
    }

    tmsPolicy.add(currentTick.time)(rulesToAdd)

    val expiredRules = signal match { //logic somewhat implicit...
      case None => expirationHandling.deregisterExpiredByTime()
      case _ => expirationHandling.deregisterExpiredByCount()
    }

    val rulesToRemove = expiredRules filterNot (rulesToAdd.contains(_)) //do not remove first; concerns efficiency of tms

    if (IEEConfig.printRules) {
      println("\nrules removed at tick " + currentTick)
      if (rulesToRemove.isEmpty) println("(none)") else {
        rulesToRemove foreach println
      }
    }

    tmsPolicy.remove(currentTick.time)(rulesToRemove)
  }

  //method to be called whenever time xor count increases by 1
//  def singleOneDimensionalTickIncrement(signal: Option[Atom] = None) {
//
//    val rulesToGround: Seq[(Expiration, NormalRule)] = incrementalRuleMaker.rulesToGroundFor(currentTick, signal)
//    rulesToGround foreach { case (e, r) =>
//      grounder.add(r)
//      expirationHandling.register(e, Set(r))
//    }
//    val rulesToAdd = rulesToGround flatMap { case (e, r) =>
//      val rules = grounder.ground(r)
//      if (!rules.isEmpty) expirationHandling.register(e, rules)
//      rules
//    }
//
//    if (IEEConfig.printRules) {
//      println("rules added at tick " + currentTick)
//      rulesToAdd foreach println
//    }
//
//    tmsPolicy.add(currentTick.time)(rulesToAdd)
//
//    val expiredRules = signal match { //logic somewhat implicit...
//      case None => expirationHandling.deregisterExpiredByTime()
//      case _ => expirationHandling.deregisterExpiredByCount()
//    }
//
//    val rulesToRemove = expiredRules filterNot (rulesToAdd.contains(_)) //do not remove first; concerns efficiency of tms
//
//    if (IEEConfig.printRules) {
//      println("\nrules removed at tick " + currentTick)
//      if (rulesToRemove.isEmpty) println("(none)") else {
//        rulesToRemove foreach println
//      }
//    }
//
//    grounder.remove(rulesToRemove)
//    tmsPolicy.remove(currentTick.time)(rulesToRemove)
//  }

  object expirationHandling {

    var rulesExpiringAtTime: Map[Long, Set[NormalRule]] = HashMap[Long, Set[NormalRule]]()
    var rulesExpiringAtCount: Map[Long, Set[NormalRule]] = HashMap[Long, Set[NormalRule]]()

    def register(expRule: ExpiringNormalRule): Unit = {
      val t = expRule.expiration.time
      val c = expRule.expiration.count
      if (t != Void) {
        rulesExpiringAtTime = rulesExpiringAtTime updated (t, rulesExpiringAtTime.getOrElse(t, Set()) + expRule.rule)
      }
      if (c != Void) {
        rulesExpiringAtCount = rulesExpiringAtCount updated (c, rulesExpiringAtCount.getOrElse(c, Set()) + expRule.rule)
      }
    }

    def register(expRules: Set[ExpiringNormalRule]) = expRules foreach register

    def deregisterExpiredByTime(): Seq[NormalRule] = {
      if (!rulesExpiringAtTime.contains(currentTick.time)) {
        return Seq()
      }
      val rules: Set[NormalRule] = rulesExpiringAtTime.get(currentTick.time).get
      rulesExpiringAtTime = rulesExpiringAtTime - currentTick.time
      rules.toSeq
    }

    def deregisterExpiredByCount(): Seq[NormalRule] = {
      if (!rulesExpiringAtCount.contains(currentTick.count)) {
        return Seq()
      }
      val rules: Set[NormalRule] = rulesExpiringAtCount.get(currentTick.count).get
      rulesExpiringAtCount = rulesExpiringAtCount - currentTick.count
      rules.toSeq
    }

  }

}

object IEEConfig {
  var printRules = false
}