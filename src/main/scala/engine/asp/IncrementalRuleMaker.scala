package engine.asp

import core._
import core.asp.{AspFact, NormalRule}
import engine.DefaultTrackedSignal
import engine.asp.tms.Pin


/**
  * Created by hb on 05.03.17.
  *
  * (This class does the pinning, grounding is done by the IncrementalEvaluationEngine)
  */
case class IncrementalRuleMaker(larsProgramEncoding: LarsProgramEncoding) {

  val R: Seq[(TicksUntilOutdated,NormalRule)] = larsProgramEncoding.larsRuleEncodings map (e => (e.ticksUntilOutdated,e.aspRule))
  val Q: Seq[(TicksUntilOutdated,NormalRule)] = larsProgramEncoding.nowAndAtNowIdentityRules map (r => (TickPair(1,Void),r))

  val (nonExpiringR,expiringR) = R partition { case (ticks,_) => ticks.time == Void && ticks.count == Void }

  /*
   * TODO separate rules:
   * a) no tick-variables
   * b) only time-tick variables
   * c) only count-tick variables
   * d) both tick-variables
   */

  def rulesToGroundFor(now: TickPair, signal: Option[Atom]): Seq[(Expiration,NormalRule)] = {
    val pinWithExp = timeCountPinned(now) _
    val facts:Seq[(Expiration,NormalRule)] = signal match {
      case Some(atom) => pinnedAtoms(DefaultTrackedSignal(atom,now))
      case None => Seq()
    }
    //note that this approach is not uniform. would be more elegant to return incremental rules with
    //tick variables plus TicksUntilOutdated, and uniformly pin them (like pin(Q) and pin(expiringR)
    val windowRules: Seq[(Expiration,NormalRule)] = larsProgramEncoding.windowAtomEncoders flatMap (_.incrementalRules(now))
    facts ++ pinWithExp(Q) ++ pinWithExp(expiringR) ++ nonExpiringR ++ windowRules
  }

  def timeCountPinned(now: TickPair)(rules: Seq[(TicksUntilOutdated,NormalRule)]): Seq[(Expiration,NormalRule)] = {
    val pin = Pin(now.time,now.count)
    rules map {
      case (ticksUntilOutdated,rule) => (now+ticksUntilOutdated, pin.ground(rule))
    }
  }

  /*
  def timePinned(time: Long)(rules: Seq[(TicksUntilOutdated,NormalRule)]) = timeCountPinned(TickPair(time,Void))(rules)

  def countPinned(count: Long)(rules: Seq[(TicksUntilOutdated,NormalRule)]) = timeCountPinned(TickPair(Void,count))(rules)
  */

  def pinnedAtoms(t: DefaultTrackedSignal): Seq[(Expiration,NormalRule)] = {
    Seq(t.timePinned, t.countPinned, t.timeCountPinned) map {
      a => (TickPair(Void,Void),AspFact[Atom](a)) //TODO reconsider outdating of signals
    }
  }

  //val signalTracker = new SignalTracker(larsProgramEncoding.maximumTimeWindowSizeInTicks, larsProgramEncoding.maximumTupleWindowSize, DefaultTrackedSignal.apply)

  //  def trackSignal(get: Atom) = {
  //    val trackedSignal = signalTracker.track(networkTime, signal)
  //  }

}
