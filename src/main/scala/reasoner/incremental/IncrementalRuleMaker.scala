package reasoner.incremental

import core._
import core.asp.{AspFact, NormalRule, UserDefinedAspRule}
import core.grounding.{Grounding, Pregrounder}
import core.lars._
import reasoner._
import reasoner.common.{DefaultTrackedSignal, LarsProgramEncoding, LarsToAspMapper, Tick}


/**
  * Created by hb on 05.03.17.
  *
  */
case class IncrementalRuleMaker(larsProgramEncoding: LarsProgramEncoding, grounder: Pregrounder = Pregrounder()) {

  def incrementalRules(tick: Tick, signal: Option[Atom]): Seq[AnnotatedNormalRule] = {

    val timeIncrease = signal.isEmpty

    val auxFacts: Seq[AnnotatedNormalRule] = if (need_tick_atoms) {
      val tickFact = tickFactAsNormalRule(TimePoint(tick.time),Value(tick.count.toInt))
      Seq(StaticRule(tickFact)) //TODO
    } else {
      Seq()
    }

    val signals: Seq[AnnotatedNormalRule] = { //TODO add expiration
      if (timeIncrease) { Seq() }
      else { pinnedAtoms(tick, DefaultTrackedSignal(signal.get, tick)) }
    }

    val pin = expiringRulesPinning(tick)

    val expiringRules: Seq[AnnotatedNormalRule] = {
      if (timeIncrease) { pin(rulesToPinForTimeIncrement) }
      else { pin(rulesToPinForCountIncrement) }
    }

    signals ++ auxFacts ++ expiringRules
  }

  //pin rules and determine expiration duration, filter out those where relation atoms do not hold
  def expiringRulesPinning(tick: Tick): (Seq[RuleWithDuration] => Seq[ExpiringRule]) = {
    val pin = Pin(tick.time,tick.count)
    def fn(rulesWithDuration: Seq[RuleWithDuration]): Seq[ExpiringRule] = {
      val pairs = rulesWithDuration map { rwd =>
        (rwd,Grounding.ensureRuleRelations(pin.groundTickVariables(rwd.rule)))
      }
      val expiringRules = pairs collect {
        case (rwd,optRule) if optRule.isDefined => {
          val pinnedRule = optRule.get
          val exp = tick + rwd.duration
          val mode = rwd.expirationMode
          rwd match {
            case xr: RuleWithTimeDurationOnly => RuleExpiringByTimeOnly(pinnedRule, exp, mode)
            case xr: RuleWithCountDurationOnly => RuleExpiringByCountOnly(pinnedRule, exp, mode)
            case xr: RuleWithDisjunctiveDuration => RuleExpiringByTimeOrCount(pinnedRule, exp, mode)
            case xr: RuleWithConjunctiveDuration => RuleExpiringByTimeAndCount(pinnedRule, exp, mode)
          }
        }
      }
      expiringRules
    }
    fn
  }

  val useSignalExpiration = true

  private def pinnedAtoms(tick: Tick, t: DefaultTrackedSignal): Seq[AnnotatedNormalRule] = {
    val rules: Seq[NormalRule] = if (needs_at_cnt_atoms) {
      Seq(AspFact[Atom](t.timePinned),AspFact[Atom](t.timeCountPinned))
    } else {
      Seq(AspFact[Atom](t.timePinned))
    }
    if (useSignalExpiration) {
      simpleSignalExpiration(rules)
    } else {
      rules map StaticRule
    }
  }

  private def simpleSignalExpiration(rules: Seq[NormalRule]): Seq[AnnotatedNormalRule] = {
    rules map (RuleExpiringByCountOnly(_,Tick(Void,10000),ExpirationOptional))
  }

  //
  //
  //

  private val __Q: Seq[NormalRule] = larsProgramEncoding.nowAndAtNowIdentityRules map { r =>
    val rule = IncrementalAspPreparation.stripPositionAtoms(r)
    val atom = ((rule.pos + rule.head) filter (!_.isInstanceOf[PinnedAtom])).head
    if (larsProgramEncoding.needGuard contains (atom.predicate)) {
      val guards = LarsToAspMapper.findGroundingGuards(larsProgramEncoding,atom)
      UserDefinedAspRule(rule.head,rule.pos ++ guards,Set()) //assume that the conjunction of all guards is always needed (as opposed to e.g., one guard per rule etc)
    } else {
      rule
    }
  }

  private val VoidTick = Tick(Void,Void)

  private val __baseRules: Seq[AnnotatedNormalRule] = larsProgramEncoding.larsRuleEncodings map { encoding =>
    val rule = IncrementalAspPreparation.stripPositionAtoms(encoding.baseRule)
    val ticks = encoding.ticksUntilOutdated()
    if (ticks == VoidTick) {
      StaticRule(rule)
    } else if (ticks.count == Void) {
      RuleWithTimeDurationOnly(rule,ticks,ExpirationOptional)
    } else if (ticks.time == Void) {
      RuleWithCountDurationOnly(rule,ticks,ExpirationOptional)
    } else {
      RuleWithDisjunctiveDuration(rule,ticks,ExpirationOptional)
    } //note that the conjunctive case does not exist for base rules
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
        case r:RuleWithTimeDurationOnly => RuleWithTimeDurationOnly(groundRule,r.duration,r.expirationMode,r.generationMode)
        case r:RuleWithCountDurationOnly => RuleWithCountDurationOnly(groundRule,r.duration,r.expirationMode,r.generationMode)
        case r:RuleWithDisjunctiveDuration => RuleWithDisjunctiveDuration(groundRule,r.duration,r.expirationMode,r.generationMode)
        case r:RuleWithConjunctiveDuration => RuleWithConjunctiveDuration(groundRule,r.duration,r.expirationMode,r.generationMode)
      }
    }
  }

  private def normalizeTickVariables(ruleWithDuration: RuleWithDuration): RuleWithDuration = {
    val r = ruleWithDuration.rule
    val newRule: NormalRule = UserDefinedAspRule(nrm(r.head), r.pos map nrm, r.neg map nrm)
    ruleWithDuration match {
      case xr:RuleWithTimeDurationOnly => RuleWithTimeDurationOnly(newRule, xr.duration, xr.expirationMode, xr.generationMode)
      case xr:RuleWithCountDurationOnly => RuleWithCountDurationOnly(newRule, xr.duration, xr.expirationMode, xr.generationMode)
      case xr:RuleWithDisjunctiveDuration => RuleWithDisjunctiveDuration(newRule, xr.duration, xr.expirationMode, xr.generationMode)
      case xr:RuleWithConjunctiveDuration => RuleWithConjunctiveDuration(newRule, xr.duration, xr.expirationMode, xr.generationMode)
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
      if (atom.isInstanceOf[RelationAtom]) {
        instantiateRelationAtom(atom, newArgs)
      } else {
        Atom(aa.predicate,newArgs)
      }
    }
  }

  private def instantiateRelationAtom(atom: Atom, args: Seq[Argument]): RelationAtom = {
    atom match {
      case ra:BinaryRelationAtom => ra.newInstance(args(0),args(1))
      case ra:TernaryRelationAtom => ra.newInstance(args(0),args(1),args(2))
      case _ => throw new RuntimeException("unknown relation atom "+atom)
    }
  }

  //!
  private val Q_prepared: Seq[RuleWithDuration] = __Q flatMap (r => grounder.groundPartially(r) map (RuleWithTimeDurationOnly(_,Tick(1,Void),ExpirationObligatory)))
  //!
  private val base_rules_with_duration_prepared: Seq[RuleWithDuration] = prepare(__base_rules_with_duration)
  //!
  private val window_rules_with_duration_prepared: Seq[RuleWithDuration] = prepare(__window_rules_with_duration)


  val staticGroundRules = ((__base_rules_static ++ __window_rules_static) flatMap (xr => grounder.groundFully(xr.rule))) ++ larsProgramEncoding.backgroundKnowledge

  private val rulesToPinForTimeIncrement = {
    ((base_rules_with_duration_prepared ++ window_rules_with_duration_prepared) filter { rwd =>
      rwd.generationMode == OnTimeIncreaseOnly || rwd.generationMode == OnTimeAndCountIncrease
    }) ++ Q_prepared
  }

  private val rulesToPinForCountIncrement = {
    (base_rules_with_duration_prepared ++ window_rules_with_duration_prepared) filter { rwd =>
      rwd.generationMode == OnCountIncreaseOnly || rwd.generationMode == OnTimeAndCountIncrease
    }
  }

  val windowAtoms = larsProgramEncoding.larsRules flatMap (_.body collect { case w:WindowAtom => w})

  val needs_at_cnt_atoms = windowAtoms exists {
    case WindowAtom(SlidingTupleWindow(_), _, _) => true
    case _ => false
  }

  val need_tick_atoms = windowAtoms exists {
    case WindowAtom(SlidingTupleWindow(_), Box, _) => true
    case _ => false
  }

}
