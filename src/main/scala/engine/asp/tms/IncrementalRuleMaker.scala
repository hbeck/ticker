package engine.asp.tms

import core._
import core.asp.{AspFact, NormalRule, UserDefinedAspRule}
import core.grounding.Grounding
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

  private val rulesToPinForTimeIncrease = {
    Q_prepared ++ ((base_rules_with_duration_prepared ++ window_rules_with_duration_prepared) filter (_.isInstanceOf[RuleWithTimeDuration]))
  }

  private val rulesToPinForCountIncrease = {
    (base_rules_with_duration_prepared ++ window_rules_with_duration_prepared) filter (_.isInstanceOf[RuleWithCountDuration])
  }

  def rulesToAddFor(currentTick: Tick, signal: Option[Atom]): Seq[AnnotatedNormalRule] = {

    val timeIncrease = signal.isEmpty

    val tick = tickFactAsNormalRule(TimePoint(currentTick.time),Value(currentTick.count.toInt))
    val auxFacts: Seq[AnnotatedNormalRule] = Seq() :+ StaticRule(tick) //TODO expiring based on max window length

    val signals: Seq[AnnotatedNormalRule] = { //TODO expiring
      if (timeIncrease) { Seq() }
      else { pinnedAtoms(DefaultTrackedSignal(signal.get, currentTick)) }
    }

    val pin = expiringRulesPinner(currentTick)

    val expiringRules: Seq[AnnotatedNormalRule] = {
      if (timeIncrease) { pin(rulesToPinForTimeIncrease) }
      else { pin(rulesToPinForCountIncrease) }
    }

    //TODO tuple-box

    signals ++ auxFacts ++ expiringRules
  }

  //pin rules and determine expiration duration, filter out those where relation atoms do not hold
  def expiringRulesPinner(now: Tick): (Seq[RuleWithDuration] => Seq[ExpiringRule]) = {
    val pin = Pin(now.time,now.count)
    def fn(rulesWithDuration: Seq[RuleWithDuration]): Seq[ExpiringRule] = {
      rulesWithDuration map { rwd =>
        (rwd,Grounding.ensureRuleRelations(pin.groundTickVariables(rwd.rule)))
      } collect {
        case (rwd,optRule) if optRule.isDefined => {
          val pinnedRule = optRule.get
          val exp = now + rwd.duration
          val mode = rwd.expirationMode
          rwd match {
            case xr: RuleWithTimeDurationOnly => RuleExpiringByTimeOnly(pinnedRule, exp, mode)
            case xr: RuleWithCountDurationOnly => RuleExpiringByCountOnly(pinnedRule, exp, mode)
            case xr: RuleWithDualDuration => RuleExpiringDually(pinnedRule, exp, mode)
          }
        }
      }
    }
    fn
  }

  def pinnedAtoms(t: DefaultTrackedSignal): Seq[AnnotatedNormalRule] = { //TODO expire
    Seq() :+ StaticRule(AspFact[Atom](t.timePinned)) :+ StaticRule(AspFact[Atom](t.timeCountPinned))
  }

}
