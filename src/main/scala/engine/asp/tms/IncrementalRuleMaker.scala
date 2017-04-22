package engine.asp.tms

import core.asp.{AspFact, NormalRule}
import core.grounding.incremental.TailoredIncrementalGrounder
import core.lars.TimePoint
import core.{Value, _}
import engine.DefaultTrackedSignal
import engine.asp._


/**
  * Created by hb on 05.03.17.
  *
  */
case class IncrementalRuleMaker(larsProgramEncoding: LarsProgramEncoding, grounder: TailoredIncrementalGrounder) {

  //
  //

  val Q: Seq[ExpiringNormalRuleTemplate] = larsProgramEncoding.nowAndAtNowIdentityRules map { r =>
    ExpiringNormalRuleTemplate(TickBasedAspToIncrementalAsp.stripPositionAtoms(r),Tick(1,Void))
  }

  val VoidTick = Tick(Void,Void)

  val baseRules: Seq[AnnotatedNormalRule] = larsProgramEncoding.larsRuleEncodings map { encoding =>
    val rule = TickBasedAspToIncrementalAsp.stripPositionAtoms(encoding.aspRule)
    val ticks = encoding.ticksUntilOutdated()
    if (ticks == VoidTick) {
      StaticNormalRule(rule)
    } else {
      OutdatingNormalRuleTemplate(rule,ticks)
    }
  }

  val windowRuleTemplates: Seq[AnnotatedNormalRule] = larsProgramEncoding.larsRuleEncodings flatMap { encoding =>
    encoding.windowAtomEncoders flatMap (_.windowRuleTemplates)
  }

  val (staticBaseRules,volatileBaseRules) = baseRules partition (_.isInstanceOf[StaticNormalRule])
  val (staticWindowRules,volatileWindowRules) = windowRuleTemplates partition (_.isInstanceOf[StaticNormalRule])

  //grounding part TODO

  val pregroundedQ = {
    val templates =???
  }

  //val (nonExpiringR,expiringR) = R partition { case (ticks,_) => ticks.time == Void && ticks.count == Void }

  //grounder.prepareStaticGroundRules(nonExpiringR map { case (_,r) => r})


  val staticGroundRules = grounder.staticGroundRules

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

  def rulesToAddFor(currentTick: Tick, signal: Option[Atom]): Seq[AnnotatedNormalRule] = {

    val timeIncrease = signal.isEmpty

    val tick = tickFactAsNormalRule(TimePoint(currentTick.time),Value(currentTick.count.toInt))
    val time = timeFactAsNormalRule(TimePoint(currentTick.time)) //TODO only if needed
    val auxFacts: Seq[AnnotatedNormalRule] = Seq(StaticNormalRule(tick),StaticNormalRule(time)) //TODO outdating based on max window length
    val signals: Seq[AnnotatedNormalRule] = //TODO outdating
      if (timeIncrease) { Seq() }
      else { pinnedAtoms(DefaultTrackedSignal(signal.get,currentTick)) }

    val pin = expirationPinningForTick(currentTick)
    val pQ = if (timeIncrease) pin(pregroundedQ) else Seq()
    val pB = pin(volatileBaseRules)
    val pW = pin(volatileWindowRules) //static vs volatile = expiring vs outdating

    signals ++ auxFacts ++ pQ ++ pB ++ pW
  }

//  @deprecated
//  def rulesToGroundFor(currentTick: Tick, signal: Option[Atom]): Seq[(Expiration,NormalRule)] = {
//    val pinWithExp = timeCountPinned(currentTick) _
//    val expTickFact: (Expiration, NormalRule) = (Tick(Void,Void),tickFactAsNormalRule(TimePoint(currentTick.time),Value(currentTick.count.toInt)))
//    val facts:Seq[(Expiration,NormalRule)] = Seq(expTickFact) ++ {
//      signal match {
//        case Some(atom) => pinnedAtomsDepr(DefaultTrackedSignal(atom,currentTick))
//        case None => Seq()
//      }
//    }
//    //val pq = if (signal.isDefined) Seq() else pinWithExp(Q) //only needed when time passes
//    val pq = pinWithExp(Q) //... but then we have to make sure non-ground rules are repeatedly considered until time increases!
//    val pr = pinWithExp(expiringR)
//    // window rules already come with expiration, instead of TicksUntilOutdated
//    val windowRules: Seq[(Expiration,NormalRule)] = larsProgramEncoding.windowAtomEncoders flatMap (_.incrementalRules(currentTick))
//    facts ++ pq ++ pr ++ nonExpiringR ++ windowRules
//  }

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

  def pinnedAtoms(t: DefaultTrackedSignal): Seq[AnnotatedNormalRule] = { //TODO outdating
    Seq(t.timePinned, t.timeCountPinned) map (a => StaticNormalRule(AspFact[Atom](a)))
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
