package engine.asp

import core._
import core.asp.{AspFact, NormalRule}
import core.lars.TimePoint
import engine.DefaultTrackedSignal
import engine.asp.tms.Pin


/**
  * Created by hb on 05.03.17.
  */
case class IncrementalRuleMaker(larsProgramEncoding: LarsProgramEncoding) {

  val R: Seq[(Expiration,NormalRule)] = larsProgramEncoding.larsRuleEncodings map (e => (e.ticksUntilOutdated,e.aspRule))
  val Q: Seq[NormalRule] = larsProgramEncoding.nowAndAtNowIdentityRules
  val W = larsProgramEncoding.windowAtomEncoders

  val (nonExpiringR,expiringR) = R partition (tuple => tuple._1.time == -1 && tuple._1.count == -1)

  //rules that cannot be pinned and thus are not expiring. i.e., ground rules (not mentioning time variables) and those which can be kept
  //due to setting their expiration to -1. this can be done for all lars rule encodings (but not incremental window rules)
  //def nonExpiringRules() = nonExpiringR
  def incrementalRulesForTime(time: TimePoint): Seq[(Expiration,NormalRule)] = {

    val pin = Pin(time)
    val nextTime: Expiration = TickPair(time.value + 1, -1)

    val pinnedQ = Q map (rule => (nextTime, pin.ground(rule)))

    val pinnedExpiringR = expiringR map {
      case (tickPair,rule) => (tickPair,pin.ground(rule))
    }

    pinnedQ ++ pinnedExpiringR ++ nonExpiringR //++ windowRules
    /* TODO hb curr: instead of putting in all window rules, use those with \dot{C} separately.
       the latter are those which can be pinned individually for each cnt-tick, and sent to the grounder
       after each signal.
     */

  }

  def factsForSignal(trackedSignal: DefaultTrackedSignal): Seq[(Expiration,NormalRule)] = {
    val pinned: Seq[(Expiration,NormalRule)] = pinnedAtoms(trackedSignal)
    //TODO
    Seq()
  }
  def incrementalRulesForSignal(trackedSignal: DefaultTrackedSignal): Seq[(Expiration,NormalRule)] = {
    //TODO
    Seq()
  }

  def pinnedAtoms(t: DefaultTrackedSignal): Seq[(Expiration,NormalRule)] = {
    Seq(t.timePinned, t.countPinned, t.timeCountPinned) map {
      a => (TickPair(-1L,-1L),AspFact[Atom](a)) //TODO reconsider outdating of signals
    }
  }

  //def empty(): Seq[(Expiration,NormalRule)] = Seq()

}
