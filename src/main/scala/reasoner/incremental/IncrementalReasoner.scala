package reasoner.incremental

import core._
import core.asp.NormalRule
import core.lars.TimePoint
import reasoner._
import reasoner.common.Tick
import reasoner.incremental.policies.JtmsPolicy
import reasoner.{Reasoner, Result, UnknownResult}

import scala.collection.immutable.HashMap

/**
  * Created by FM, HB on Feb/Mar 2017.
  *
  * (This class coordinates pinning (within IncrementalRuleMaker) and (then) grounding (IncrementalGrounder))
  */
case class IncrementalReasoner(incrementalRuleMaker: IncrementalRuleMaker, tmsPolicy: JtmsPolicy) extends Reasoner {

  tmsPolicy.initialize(incrementalRuleMaker.staticGroundRules)

  //time of the truth maintenance network due to previous append and result calls
  var currentTick = Tick(0, 0) //using (-1,0), first "+" will fail!
  incrementTick() //...therefore, surpass the increment and generate groundings for (0,0)

  override def append(timepoint: TimePoint)(atoms: Atom*) {
    if (timepoint.value < currentTick.time) {
      throw new RuntimeException("cannot append signal at past time t=" + timepoint + ". system time already at t'=" + currentTick.time)
    }
    updateToTimePoint(timepoint)
    atoms foreach addSignalAtCurrentTime
  }

  override def evaluate(timepoint: TimePoint): Result = {
    if (timepoint.value < currentTick.time) {
      return new UnknownResult("cannot evaluate past time t=" + timepoint + ". system time already at t'=" + currentTick.time)
    }
    updateToTimePoint(timepoint)
    tmsPolicy.getModel(timepoint)
  }

  //
  //
  //

  def updateToTimePoint(time: TimePoint) {
    if (time.value > currentTick.time) {
      for (t <- (currentTick.time + 1) to (time.value)) {
        singleTimeIncrementTo(t)
      }
    }
  }

  def singleTimeIncrementTo(time: Long) {
    currentTick = currentTick.incrementTime()
    incrementTick()
  }

  def addSignalAtCurrentTime(signal: Atom) {
    currentTick = currentTick.incrementCount()
    incrementTick(Some(signal))
  }

  def incrementTick(signal: Option[Atom] = None) {

    val annotatedRules: Seq[AnnotatedNormalRule] = incrementalRuleMaker.incrementalRules(currentTick, signal)
    annotatedRules foreach {
      case xr: ExpiringRule => expiration.registerExpirationSingleDimension(xr.rule, xr.expiration)
      case _ =>
    }
    val rulesToAdd = annotatedRules map (_.rule) toVector

    tmsPolicy.add(currentTick.time)(rulesToAdd)

    val expiredRules = signal match {
      //logic somewhat implicit...
      case None => expiration.deregisterExpiredByTime()
      case _ => expiration.deregisterExpiredByCount()
    }

    tmsPolicy.remove(currentTick.time)(expiredRules)
  }

  object expiration {

    var rulesExpiringAtTime: Map[Long, Set[NormalRule]] = HashMap[Long, Set[NormalRule]]()
    var rulesExpiringAtCount: Map[Long, Set[NormalRule]] = HashMap[Long, Set[NormalRule]]()

    def registerExpirationSingleDimension(rule: NormalRule, expiration: Tick): Unit = {
      val t = expiration.time
      val c = expiration.count
      if (t != Void) {
        rulesExpiringAtTime = rulesExpiringAtTime.updated(t, rulesExpiringAtTime.getOrElse(t, Set()) + rule)
      }
      if (c != Void) {
        rulesExpiringAtCount = rulesExpiringAtCount.updated(c, rulesExpiringAtCount.getOrElse(c, Set()) + rule)
      }
    }

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

  } //end object expiration

}