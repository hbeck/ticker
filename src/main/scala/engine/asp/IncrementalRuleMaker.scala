package engine.asp

import core._
import core.asp.{AspFact, NormalRule}
import core.lars.TimePoint
import engine.DefaultTrackedSignal
import engine.asp.tms.Pin


/**
  * Created by hb on 05.03.17.
  *
  * (This class does the pinning, grounding is done by the IncrementalEvaluationEngine)
  */
case class IncrementalRuleMaker(larsProgramEncoding: LarsProgramEncoding) {

  val R: Seq[(TicksUntilOutdated,NormalRule)] = larsProgramEncoding.larsRuleEncodings map (e => (e.ticksUntilOutdated,e.aspRule))
  val Q: Seq[(TicksUntilOutdated,NormalRule)] = larsProgramEncoding.nowAndAtNowIdentityRules map (r => (TickPair(1,-1),r))

  val (nonExpiringR,expiringR) = R partition { case (ticks,_) => ticks.time == -1 && ticks.count == -1 }

  /*
   * TODO separate rules:
   * a) no tick-variables
   * b) only time-tick variables
   * c) only count-tick variables
   * d) both tick-variables
   */

  def allRulesToGroundForTime(time: TimePoint): Seq[(Expiration,NormalRule)] = {
    val pin = timePinned(time) _
    //note that this approach is not uniform. would be more elegant to return incremental rules with
    //tick variables plus TicksUntilOutdated, and uniformly pin them (like pin(Q) and pin(expiringR)
    val nowTick = TickPair(time.value,-1)
    val windowRules: Seq[(Expiration,NormalRule)] = larsProgramEncoding.windowAtomEncoders flatMap (_.incrementalRules(nowTick))
    pin(Q) ++ pin(expiringR) ++ nonExpiringR ++ windowRules
  }

  def allRulesToGroundForSignal(trackedSignal: DefaultTrackedSignal): Seq[(Expiration,NormalRule)] = {
    val pin = timeCountPinned(trackedSignal.time, trackedSignal.count) _
    val facts = pinnedAtoms(trackedSignal)
    //note that this approach is not uniform. would be more elegant to return incremental rules with
    //tick variables plus TicksUntilOutdated, and uniformly pin them (like pin(Q) and pin(expiringR)
    val nowTick = TickPair(trackedSignal.time.value,trackedSignal.count)
    val windowRules: Seq[(Expiration,NormalRule)] = larsProgramEncoding.windowAtomEncoders flatMap (_.incrementalRules(nowTick))
    facts ++ pin(Q) ++ pin(expiringR) ++ nonExpiringR ++ windowRules
  }

  //
  //

  def timeCountPinned(time: TimePoint, count: Long)(rules: Seq[(TicksUntilOutdated,NormalRule)]): Seq[(Expiration,NormalRule)] = {
    val pin = Pin(time,count)
    val tick = TickPair(time.value,count)
    rules map {
      case (ticksUntilOutdated,rule) => (tick+ticksUntilOutdated, pin.ground(rule))
    }
  }

  def timePinned(time: TimePoint)(rules: Seq[(TicksUntilOutdated,NormalRule)]) = timeCountPinned(time,-1)(rules)

  def countPinned(count: Long)(rules: Seq[(TicksUntilOutdated,NormalRule)]) = timeCountPinned(TimePoint(-1),count)(rules)

  def pinnedAtoms(t: DefaultTrackedSignal): Seq[(Expiration,NormalRule)] = {
    Seq(t.timePinned, t.countPinned, t.timeCountPinned) map {
      a => (TickPair(-1L,-1L),AspFact[Atom](a)) //TODO reconsider outdating of signals
    }
  }

}
