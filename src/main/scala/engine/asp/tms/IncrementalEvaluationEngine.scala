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

  tmsPolicy.initialize(incrementalRuleMaker.staticGroundRules)

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

    val annotatedRules: Seq[AnnotatedNormalRule] = incrementalRuleMaker.rulesToAddFor(currentTick, signal)
    annotatedRules foreach {
      case xr: RuleExpiringByTimeAndCount => expiration.registerExpirationBothDimensions(xr)
      case xr: ExpiringRule => expiration.registerExpirationSingleDimension(xr.rule, xr.expiration)
      case _ =>
    }
    val rulesToAdd = annotatedRules map (_.rule) toVector

    if (IEEConfig.printRules) {
      println("rules added at tick " + currentTick)
      rulesToAdd foreach println
    }

    tmsPolicy.add(currentTick.time)(rulesToAdd)

    val expiredRules = (signal match { //TODO
      //logic somewhat implicit...
      case None => expiration.deregisterExpiredByTime()
      case _ => expiration.deregisterExpiredByCount()
    }) ++ (if (incrementalRuleMaker.useSignalExpiration) {
      expiration.deregisterExpiredByTimeAndCount()
    } else {
      Seq()
    })

    //val addLookup = rulesToAdd.toSet
    //val rulesToRemove = expiredRules //filterNot addLookup.contains //do not remove first; concerns efficiency of tms

    if (IEEConfig.printRules) {
      println("\nrules removed at tick " + currentTick)
      //      if (rulesToRemove.isEmpty) println("(none)") else {
      //        rulesToRemove foreach println
      //      }
      if (expiredRules.isEmpty) println("(none)") else {
        expiredRules foreach println
      }
    }

    //tmsPolicy.remove(currentTick.time)(rulesToRemove)
    tmsPolicy.remove(currentTick.time)(expiredRules)
  }

  object expiration {

    var rulesExpiringAtTime: Map[Long, Set[NormalRule]] = HashMap[Long, Set[NormalRule]]()
    var rulesExpiringAtCount: Map[Long, Set[NormalRule]] = HashMap[Long, Set[NormalRule]]()

    var rulesDualExpirationTimeIndex: Map[Long, Set[RuleExpiringByTimeAndCount]] = HashMap[Long,Set[RuleExpiringByTimeAndCount]]()
    var rulesDualExpirationCountIndex: Map[Long, Set[RuleExpiringByTimeAndCount]] = HashMap[Long,Set[RuleExpiringByTimeAndCount]]()

    def registerExpirationSingleDimension(rule: NormalRule, expiration: Tick): Unit = {
      val t = expiration.time
      val c = expiration.count
      if (t != Void) {
        rulesExpiringAtTime = rulesExpiringAtTime updated (t, rulesExpiringAtTime.getOrElse(t, Set()) + rule)
      }
      if (c != Void) {
        rulesExpiringAtCount = rulesExpiringAtCount updated (c, rulesExpiringAtCount.getOrElse(c, Set()) + rule)
      }
    }

    def registerExpirationBothDimensions(xRule: RuleExpiringByTimeAndCount): Unit = {
      val t = xRule.expiration.time
      val c = xRule.expiration.count
      rulesDualExpirationTimeIndex = rulesDualExpirationTimeIndex updated (t, rulesDualExpirationTimeIndex.getOrElse(t, Set()) + xRule)
      rulesDualExpirationCountIndex = rulesDualExpirationCountIndex updated (c, rulesDualExpirationCountIndex.getOrElse(c, Set()) + xRule)
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

    //ugly but efficient...
    def deregisterExpiredByTimeAndCount(): Seq[NormalRule] = {

      //delete both entries when both dimensions are expired
      val t = currentTick.time
      val c = currentTick.count
      val mayExpireAtT = rulesDualExpirationTimeIndex.getOrElse(t,Set())
      val mayExpireAtC = rulesDualExpirationCountIndex.getOrElse(c,Set())
      if (mayExpireAtT.isEmpty) {
        if (mayExpireAtC.isEmpty) {
          return Seq()
        } else {
          val (expired,toKeep) = mayExpireAtC partition (_.expiration.time > t) //cannot be == t, otherwise mayExpireAtT nonEmpty
          countBasedRemoval(expired,toKeep)
          return expired map (_.rule) toSeq
        }
      } else { //mayExpireAtT is not empty
        if (mayExpireAtC.isEmpty) {
          val (expired,toKeep) = mayExpireAtT partition (_.expiration.count > c) //cannot be == c, otherwise mayExpireAtC nonEmpty
          timeBasedRemoval(expired,toKeep)
          return expired map (_.rule) toSeq
        } else { //mayExpireAtC is not empty, i.e., some rules are expired by both dimensions at the same 'time'
          val (expiredT,toKeepT) = mayExpireAtT partition (_.expiration.count >= c)
          timeBasedRemoval(expiredT,toKeepT)
          val (expiredC,toKeepC) = mayExpireAtC partition (_.expiration.time >= t)
          countBasedRemoval(expiredC,toKeepC)
          return (expiredT ++ expiredC) map (_.rule) toSeq
        }
      }

    } //end deregisterByTimeAndCount

    private def countBasedRemoval(expired: Set[RuleExpiringByTimeAndCount], toKeep: Set[RuleExpiringByTimeAndCount]): Unit = {
      rulesDualExpirationCountIndex = rulesDualExpirationCountIndex updated (currentTick.count, toKeep)
      expired foreach { xRule =>
        val newSetForExpirationTime = rulesDualExpirationTimeIndex.getOrElse(xRule.expiration.time,Set()) - xRule
        if (newSetForExpirationTime.isEmpty) {
          rulesDualExpirationTimeIndex = rulesDualExpirationTimeIndex - xRule.expiration.time
        } else {
          rulesDualExpirationTimeIndex = rulesDualExpirationTimeIndex updated (currentTick.time, newSetForExpirationTime)
        }
      }
    }

    private def timeBasedRemoval(expired: Set[RuleExpiringByTimeAndCount], toKeep: Set[RuleExpiringByTimeAndCount]): Unit = {
      rulesDualExpirationTimeIndex = rulesDualExpirationTimeIndex updated (currentTick.time, toKeep)
      expired foreach { xRule =>
        val newSetForExpirationCount = rulesDualExpirationCountIndex.getOrElse(xRule.expiration.count,Set()) - xRule
        if (newSetForExpirationCount.isEmpty) {
          rulesDualExpirationCountIndex = rulesDualExpirationCountIndex - xRule.expiration.count
        } else {
          rulesDualExpirationCountIndex = rulesDualExpirationCountIndex updated (currentTick.count, newSetForExpirationCount)
        }
      }
    }

  } //end object expiration

}

object IEEConfig {
  var printRules = false
}