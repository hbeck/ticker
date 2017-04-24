package engine.asp.tms

import core._
import core.asp.{AspFact, NormalRule, UserDefinedAspRule}
import core.grounding.incremental.TailoredIncrementalGrounder
import core.lars._
import engine.DefaultTrackedSignal
import engine.asp._


/**
  * Created by hb on 05.03.17.
  *
  */
case class IncrementalRuleMaker(larsProgramEncoding: LarsProgramEncoding, grounder: TailoredIncrementalGrounder = TailoredIncrementalGrounder()) {

  private val __Q: Seq[NormalRule] = larsProgramEncoding.nowAndAtNowIdentityRules map { r =>
    val rule = TickBasedAspToIncrementalAsp.stripPositionAtoms(r)
    val atom = ((rule.pos + rule.head) filter (!_.isInstanceOf[PinnedAtom])).head
    if (larsProgramEncoding.needGuard contains (atom.predicate)) {
      val guards = LarsToAspMapper.findGroundingGuards(larsProgramEncoding,atom)
      UserDefinedAspRule(rule.head,rule.pos ++ guards,Set()) //assume that the conjunction of all guards is always needed (as opposed to e.g., one guaard per rule etc)
    } else {
      rule
    }
  }

  private val VoidTick = Tick(Void,Void)

  private val __baseRules: Seq[AnnotatedNormalRule] = larsProgramEncoding.larsRuleEncodings map { encoding =>
    val rule = TickBasedAspToIncrementalAsp.stripPositionAtoms(encoding.aspRule)
    val ticks = encoding.ticksUntilOutdated()
    if (ticks == VoidTick) {
      StaticRule(rule)
    } else if (ticks.count == Void) {
      RuleWithTimeDurationOnly(rule,ticks,ExpirationOptional)
    } else if (ticks.time == Void) {
      RuleWithCountDurationOnly(rule,ticks,ExpirationOptional)
    } else {
      RuleWithDualDuration(rule,ticks,ExpirationOptional)
    }
  }

  private val __windowRules: Seq[AnnotatedNormalRule] = larsProgramEncoding.larsRuleEncodings flatMap { encoding =>
    encoding.windowAtomEncoders flatMap (_.windowRuleTemplates)
  }

  private val (__base_rules_static,__base_rules_with_duration) = __baseRules partition (_.isInstanceOf[StaticRule])
  private val (__window_rules_static,__window_rules_with_duration) = __windowRules partition (_.isInstanceOf[StaticRule])

  private val __allRules: Seq[NormalRule] = ((__baseRules ++ __windowRules) map (_.rule)) ++ __Q ++ larsProgramEncoding.backgroundKnowledge

  grounder.init(__allRules)

  private def prepare(xRules: Seq[AnnotatedNormalRule]): Seq[RuleWithDuration] = groundPartially(xRules) map normalizeTickVariables

  private def groundPartially(xRules: Seq[AnnotatedNormalRule]): Seq[RuleWithDuration] = xRules flatMap { xr =>
    val rwd = xr.asInstanceOf[RuleWithDuration]
    grounder.groundPartially(rwd.rule) map { groundRule =>
      rwd match {
        case r:RuleWithTimeDurationOnly => RuleWithTimeDurationOnly(groundRule,r.duration,r.expirationMode,r.preparationMode)
        case r:RuleWithCountDurationOnly => RuleWithCountDurationOnly(groundRule,r.duration,r.expirationMode,r.preparationMode)
        case r:RuleWithDualDuration => RuleWithDualDuration(groundRule,r.duration,r.expirationMode,r.preparationMode)
      }
    }
  }

  private def normalizeTickVariables(ruleWithDuration: RuleWithDuration): RuleWithDuration = {
    val r = ruleWithDuration.rule
    val h = r.head
    val p = r.pos
    val n = r.neg
    val newRule: NormalRule = UserDefinedAspRule(nrm(h), p map nrm, n map nrm)
    ruleWithDuration match {
      case xr:RuleWithTimeDurationOnly => RuleWithTimeDurationOnly(newRule, xr.duration, xr.expirationMode, xr.preparationMode)
      case xr:RuleWithCountDurationOnly => RuleWithCountDurationOnly(newRule, xr.duration, xr.expirationMode, xr.preparationMode)
      case xr:RuleWithDualDuration => RuleWithDualDuration(newRule, xr.duration, xr.expirationMode, xr.preparationMode)
    }
  }

  private def nrm(atom: Atom): Atom = {
    if (atom.isGround()) { atom }
    else {
      val aa = atom.asInstanceOf[AtomWithArguments]
      val newArgs = aa.arguments map {
        case v@TimeVariableWithOffset(variable,offset) => if (variable.name == TimePinVariableName) v else TimeVariableWithOffset(Variable(TimePinVariableName),offset)
        case v@VariableWithOffset(variable,offset) => if (variable.name == CountPinVariableName) v else VariableWithOffset(Variable(CountPinVariableName),offset)
        case v@StringVariable(name) => if (name == CountPinVariableName) v else StringVariable(CountPinVariableName)
        case arg => arg
      }
      Atom(aa.predicate,newArgs)
    }
  }

  //!
  private val Q_prepared: Seq[RuleWithDuration] = __Q flatMap (r => grounder.groundPartially(r) map (RuleWithTimeDurationOnly(_,Tick(1,Void),ExpirationObligatory)))

  //!
  private val base_rules_with_duration_prepared: Seq[RuleWithDuration] = prepare(__base_rules_with_duration)

  private val window_rules_with_duration_partial_grounding: Seq[RuleWithDuration] = prepare(__window_rules_with_duration)

  //first are from tuple-box combination. to be tackled manually
  //!
  private val (window_rules_with_duration_prepared, window_rules_with_duration_needing_incremental_grounding) = {
    window_rules_with_duration_partial_grounding partition (_.preparationMode == MayBePregrounded)
  }

  val staticGroundRules = (__base_rules_static ++ __window_rules_static) flatMap (xr => grounder.groundFully(xr.rule))

  private val rulesToPinForTimeIncrease =
    Q_prepared ++ ((base_rules_with_duration_prepared ++ window_rules_with_duration_prepared) filter (_.isInstanceOf[RuleWithTimeDuration]))

  private val rulesToPinForCountIncrease =
    (base_rules_with_duration_prepared ++ window_rules_with_duration_prepared) filter (_.isInstanceOf[RuleWithCountDuration])

  def rulesToAddFor(currentTick: Tick, signal: Option[Atom]): Seq[AnnotatedNormalRule] = {

    val timeIncrease = signal.isEmpty

    val tick = tickFactAsNormalRule(TimePoint(currentTick.time),Value(currentTick.count.toInt))
    val auxFacts: Seq[AnnotatedNormalRule] = Seq() :+ StaticRule(tick) //TODO expiring based on max window length

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

  def expirationPinningForTick(now: Tick): (Seq[RuleWithDuration] => Seq[ExpiringRule]) = {
    val pin = Pin(now.time,now.count)
    def f(rules: Seq[RuleWithDuration]): Seq[ExpiringRule] = rules map {
      case xr: RuleWithTimeDurationOnly => RuleExpiringByTimeOnly(pin.groundTickVariables(xr.rule), now + xr.duration, xr.expirationMode)
      case xr: RuleWithCountDurationOnly => RuleExpiringByCountOnly(pin.groundTickVariables(xr.rule), now + xr.duration, xr.expirationMode)
      case xr: RuleWithDualDuration => RuleExpiringDually(pin.groundTickVariables(xr.rule), now + xr.duration, xr.expirationMode)
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
    Seq() :+ StaticRule(AspFact[Atom](t.timePinned)) :+ StaticRule(AspFact[Atom](t.timeCountPinned))
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
