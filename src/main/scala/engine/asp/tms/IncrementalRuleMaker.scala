package engine.asp.tms

import core.asp._
import core.asp.{AspFact, NormalRule}
import core.grounding.incremental.TailoredIncrementalGrounder
import core.lars.TimePoint
import core.{Value, _}
import engine.DefaultTrackedSignal
import engine.asp.{Expiration, LarsProgramEncoding, Tick, TicksUntilOutdated, Void, tickFactAsNormalRule}


/**
  * Created by hb on 05.03.17.
  *
  */
case class IncrementalRuleMaker(larsProgramEncoding: LarsProgramEncoding, grounder: TailoredIncrementalGrounder) {

  val VoidTick = Tick(Void,Void)

  //
  //

  val R: Seq[(TicksUntilOutdated,NormalRule)] = larsProgramEncoding.larsRuleEncodings map { encoding =>
    val rule = TickBasedAspToIncrementalAsp.stripPositionAtoms(encoding.aspRule) //now(.), cnt(.) should not be there anyway
    (encoding.ticksUntilOutdated,rule)
  }
  val Q: Seq[(TicksUntilOutdated,NormalRule)] = larsProgramEncoding.nowAndAtNowIdentityRules map { r =>
    (Tick(1,Void),TickBasedAspToIncrementalAsp.stripPositionAtoms(r))
  }

  val (nonExpiringR,expiringR) = R partition { case (ticks,_) => ticks.time == Void && ticks.count == Void }

  grounder.prepareStaticGroundRules(nonExpiringR map { case (_,r) => r})
  grounder.ground()

  val staticGroundRules = grounder.staticGroundRules

  //val (groundNonExpiringR,nonGroundNonExpiringR) = nonExpiringR partition { case (_,r) => r.isGround }

  //val staticGroundRules = groundNonExpiringR map { case (_,r) => r } //TODO add those of window atom encoders later

  //TODO 0420 all rules that do not depend on ticks: some of window atom encodings (box)
  //TODO 0420 what about those that depend on ticks. add pre-grounding based on (0,0)?
  //val (tickDependentBaseRules,staticRules): (Seq[NormalRule],Seq[NormalRule]) = larsProgramEncoding.baseRules partition hasTickVariable

  def hasTickVariable(rule: NormalRule) = rule.atoms.exists { a =>
    a.isInstanceOf[PinnedAtom] && a.asInstanceOf[PinnedAtom].pinnedArguments.exists(arg => arg.isInstanceOf[Variable])
  }

  /*
   * TODO separate rules:
   * a) no tick-variables
   * b) only time-tick variables
   * c) only count-tick variables
   * d) both tick-variables
   */

  def rulesToAddFor(currentTick: Tick, signal: Option[Atom]): Seq[ExpiringNormalRule] = {

    val tick = tickFactAsNormalRule(TimePoint(currentTick.time),Value(currentTick.count.toInt)) //TODO also 'time' if needed
    val eTick = StaticNormalRule(tick) //TODO outdating based on max window length
    val facts: Seq[AnnotatedNormalRule] = Seq(eTick) ++ {
      signal match {
        case Some(atom) => pinnedAtoms(DefaultTrackedSignal(atom,currentTick))
        case None => Seq()
      }
    }

    val pinning = expirationPinningForTick(currentTick)


  }

  @deprecated
  def rulesToGroundFor(currentTick: Tick, signal: Option[Atom]): Seq[(Expiration,NormalRule)] = {
    val pinWithExp = timeCountPinned(currentTick) _
    val expTickFact: (Expiration, NormalRule) = (Tick(Void,Void),tickFactAsNormalRule(TimePoint(currentTick.time),Value(currentTick.count.toInt)))
    val facts:Seq[(Expiration,NormalRule)] = Seq(expTickFact) ++ {
      signal match {
        case Some(atom) => pinnedAtomsDepr(DefaultTrackedSignal(atom,currentTick))
        case None => Seq()
      }
    }
    //val pq = if (signal.isDefined) Seq() else pinWithExp(Q) //only needed when time passes
    val pq = pinWithExp(Q) //... but then we have to make sure non-ground rules are repeatedly considered until time increases!
    val pr = pinWithExp(expiringR)
    // window rules already come with expiration, instead of TicksUntilOutdated
    val windowRules: Seq[(Expiration,NormalRule)] = larsProgramEncoding.windowAtomEncoders flatMap (_.incrementalRules(currentTick))
    facts ++ pq ++ pr ++ nonExpiringR ++ windowRules
  }

  def expirationPinningForTick(now: Tick): (Seq[ExpiringNormalRuleTemplate] => Seq[ExpiringNormalRule]) = {
    val pin = Pin(now.time,now.count)
    def f(rules: Seq[ExpiringNormalRuleTemplate]): Seq[ExpiringNormalRule] = {
      rules map (r => ExpiringNormalRule(pin.groundTickVariables(r.rule),now+r.ticksUntilExpired))
    }
    f
  }

  @deprecated
  def timeCountPinned(now: Tick)(rules: Seq[(TicksUntilOutdated,NormalRule)]): Seq[(Expiration,NormalRule)] = {
    val pin = Pin(now.time,now.count)
    rules map {
      case (ticksUntilOutdated,rule) => (now+ticksUntilOutdated, pin.groundTickVariables(rule))
    }
  }

  /*
  def timePinned(time: Long)(rules: Seq[(TicksUntilOutdated,NormalRule)]) = timeCountPinned(TickPair(time,Void))(rules)

  def countPinned(count: Long)(rules: Seq[(TicksUntilOutdated,NormalRule)]) = timeCountPinned(TickPair(Void,count))(rules)
  */

  def pinnedAtoms(t: DefaultTrackedSignal): Seq[ExpiringNormalRule] = { //TODO outdating
    Seq(t.timePinned, t.timeCountPinned) map (a => ExpiringNormalRule(AspFact[Atom](a),VoidTick))
  }

  @deprecated
  def pinnedAtomsDepr(t: DefaultTrackedSignal): Seq[(Expiration,NormalRule)] = {
    // Seq(t.timePinned, t.countPinned, t.timeCountPinned) map {
    Seq(t.timePinned, t.timeCountPinned) map {
      a => (Tick(Void,Void),AspFact[Atom](a)) //TODO reconsider outdating of signals
    }
  }

  //val signalTracker = new SignalTracker(larsProgramEncoding.maximumTimeWindowSizeInTicks, larsProgramEncoding.maximumTupleWindowSize, DefaultTrackedSignal.apply)

  //  def trackSignal(get: Atom) = {
  //    val trackedSignal = signalTracker.track(networkTime, signal)
  //  }

}
