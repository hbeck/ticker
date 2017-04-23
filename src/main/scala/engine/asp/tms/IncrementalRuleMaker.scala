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

  private val Q: Seq[NormalRule] = larsProgramEncoding.nowAndAtNowIdentityRules map (TickBasedAspToIncrementalAsp.stripPositionAtoms(_))

  private val VoidTick = Tick(Void,Void)

  private val baseRules: Seq[AnnotatedNormalRule] = larsProgramEncoding.larsRuleEncodings map { encoding =>
    val rule = TickBasedAspToIncrementalAsp.stripPositionAtoms(encoding.aspRule)
    val ticks = encoding.ticksUntilOutdated()
    if (ticks == VoidTick) {
      StaticNormalRule(rule)
    } else if (ticks.count == Void) {
      NormalRuleWithTimeDuration(rule,ticks,ExpirationOptional)
    } else if (ticks.time == Void) {
      NormalRuleWithCountDuration(rule,ticks,ExpirationOptional)
    } else {
      NormalRuleWithDualDuration(rule,ticks,ExpirationOptional)
    }
  }

  private val windowRules: Seq[AnnotatedNormalRule] = larsProgramEncoding.larsRuleEncodings flatMap { encoding =>
    encoding.windowAtomEncoders flatMap (_.windowRuleTemplates)
  }

  private val (baseRulesStatic,baseRulesWithDuration) = baseRules partition (_.isInstanceOf[StaticNormalRule])
  private val (windowRulesStatic,windowRulesWithDuration) = windowRules partition (_.isInstanceOf[StaticNormalRule])

  private val allRules: Seq[NormalRule] = ((baseRules ++ windowRules) map (_.rule)) ++ Q ++ larsProgramEncoding.backgroundKnowledge

  grounder.init(allRules)

  private def groundFully(xRules: Seq[AnnotatedNormalRule]): Seq[NormalRule] = xRules flatMap (xr => grounder.groundFully(xr.rule))

  private def groundPartially(xRules: Seq[AnnotatedNormalRule]): Seq[NormalRuleWithDuration] = xRules flatMap { xr =>
    val rwd = xr.asInstanceOf[NormalRuleWithDuration]
    grounder.groundPartially(rwd.rule) map { groundRule =>
      rwd match {
        case r:NormalRuleWithTimeDuration => NormalRuleWithTimeDuration(groundRule,r.duration,r.expirationMode,r.preparationMode)
        case r:NormalRuleWithCountDuration => NormalRuleWithCountDuration(groundRule,r.duration,r.expirationMode,r.preparationMode)
        case r:NormalRuleWithDualDuration => NormalRuleWithDualDuration(groundRule,r.duration,r.expirationMode,r.preparationMode)
      }
    }
  }


  //!
  private val Q_prepared: Seq[NormalRuleWithDuration] = Q flatMap (r => grounder.groundPartially(r) map (NormalRuleWithTimeDuration(_,Tick(1,Void),ExpirationObligatory)))

  //!
  private val baseRules_expiring_prepared: Seq[NormalRuleWithDuration] = groundPartially(baseRulesWithDuration)

  private val windowRulesExpiringPartialGrounding: Seq[NormalRuleWithDuration] = groundPartially(windowRulesWithDuration)

  //first are from tuple-box combination. to be tackled manually
  //!
  private val (windowRules_expiring_prepared, windowRules_expiring_that_need_incremental_grounding) = {
    windowRulesExpiringPartialGrounding partition (_.preparationMode == MayBePregrounded)
  }

  val staticGroundRules = groundFully(baseRulesStatic ++ windowRulesStatic)

  def rulesToAddFor(currentTick: Tick, signal: Option[Atom]): Seq[AnnotatedNormalRule] = {

    val timeIncrease = signal.isEmpty

    val tick = tickFactAsNormalRule(TimePoint(currentTick.time),Value(currentTick.count.toInt))
    val auxFacts: Seq[AnnotatedNormalRule] = Seq() :+ StaticNormalRule(tick) //TODO expiring based on max window length
    val signals: Seq[AnnotatedNormalRule] = { //TODO expiring
      if (timeIncrease) { Seq() }
      else { pinnedAtoms(DefaultTrackedSignal(signal.get, currentTick)) }
    }

    val pin = expirationPinningForTick(currentTick)
    val expiringRules: Seq[AnnotatedNormalRule] = {
      if (timeIncrease) { pin(rulesToPinForTimeIncrease) }
      else { pin(rulesToPinForCountIncrease) }
    }

    //TODO tuple-box

    signals ++ auxFacts ++ expiringRules
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

  def expirationPinningForTick(now: Tick): (Seq[NormalRuleWithDuration] => Seq[ExpiringNormalRule]) = {
    val pin = Pin(now.time,now.count)
    def f(rules: Seq[NormalRuleWithDuration]): Seq[ExpiringNormalRule] = rules map {
      case xr: NormalRuleWithTimeDuration => NormalRuleTimeExpiration(pin.groundTickVariables(xr.rule), now + xr.duration, xr.expirationMode)
      case xr: NormalRuleWithCountDuration => NormalRuleCountExpiration(pin.groundTickVariables(xr.rule), now + xr.duration, xr.expirationMode)
      case xr: NormalRuleWithDualDuration => NormalRuleDualExpiration(pin.groundTickVariables(xr.rule), now + xr.duration, xr.expirationMode)
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

  def pinnedAtoms(t: DefaultTrackedSignal): Seq[AnnotatedNormalRule] = { //TODO expire
    Seq() :+ StaticNormalRule(AspFact[Atom](t.timePinned)) :+ StaticNormalRule(AspFact[Atom](t.timeCountPinned))
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
