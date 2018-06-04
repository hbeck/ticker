package reasoner.incremental

import core._
import core.asp.{NormalRule, UserDefinedAspRule}
import core.grounding.{Grounding, Pregrounder}
import core.lars._
import reasoner._
import reasoner.common._


/**
  * Created by hb on 05.03.17.
  *
  */
case class IncrementalRuleMaker(larsProgramEncoding: LarsProgramEncoding, grounder: Pregrounder = Pregrounder()) {

  def incrementalRules(tick: Tick, signal: Option[Atom]): Seq[ExpiringRule] = {

    val timeIncrement = signal.isEmpty

    val tickStreamFacts: Seq[RuleWithDuration] = tickStreamEncoding(tick, signal)

    val pin = expiringRulesPinning(tick)

    val expiringRules: Seq[ExpiringRule] = {
      if (timeIncrement) { pin(rulesToPinForTimeIncrement) }
      else { pin(rulesToPinForCountIncrement) }
    }

    pin(tickStreamFacts) ++ expiringRules
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

  private def tickStreamEncoding(tick: Tick, signal: Option[Atom]): Seq[RuleWithDuration] = {

    var facts: Seq[RuleWithDuration] = if (need_tick_atoms) {
      val tickFact = tickFactAsNormalRule(TimePoint(tick.time),Value(tick.count.toInt))
      Seq(RuleWithCountDurationOnly(tickFact,Tick(Void,maxTupleBoxSize),ExpirationOptional,OnCountIncreaseOnly))
    } else {
      Seq()
    }

    val timeIncrement = signal.isEmpty
    if (timeIncrement) {
      return facts
    }

    val atom = DefaultTrackedSignal(signal.get, tick)

    //a(x)
    facts = facts :+ RuleWithTimeDurationOnly(atom.signalFact,Tick(1,Void),ExpirationObligatory,OnCountIncreaseOnly)
    if (hasTupleWindow) {
      //a(x,t,c)
      facts = facts :+ RuleWithConjunctiveDuration(atom.tickPinnedFact,Tick(1,maxTupleWindowSize),ExpirationOptional,OnCountIncreaseOnly)
      //a(x,t)
      if (hasTupleBoxCombination) {
        if (hasTimeWindow) {
          facts = facts :+ RuleWithConjunctiveDuration(atom.timePinnedFact,Tick(maxTimeWindowSize+1,maxTupleBoxSize),ExpirationOptional,OnCountIncreaseOnly)
        } else {
          facts = facts :+ RuleWithCountDurationOnly(atom.timePinnedFact,Tick(Void,maxTupleBoxSize),ExpirationOptional,OnCountIncreaseOnly)
        }
      } else if (hasTimeWindow) {
        facts = facts :+ RuleWithTimeDurationOnly(atom.timePinnedFact,Tick(maxTimeWindowSize+1,Void),ExpirationOptional,OnCountIncreaseOnly)
      }
    } else if (hasTimeWindow) {
      facts = facts :+ RuleWithTimeDurationOnly(atom.timePinnedFact,Tick(maxTimeWindowSize+1,Void),ExpirationOptional,OnCountIncreaseOnly)
    }

    facts
  }

  //
  //
  //

  private val __Q: Seq[NormalRule] = larsProgramEncoding.nowAndAtNowIdentityRules map { r =>
    val rule = IncrementalAspPreparation.stripPositionAtoms(r)
    val atom = ((rule.pos + rule.head) filter (!_.isInstanceOf[PinnedAtom])).head
    if (larsProgramEncoding.needGuard.contains(atom.predicate)) {
      val guards = LarsToAspMapper.findGroundingGuards(larsProgramEncoding,atom)
      UserDefinedAspRule(rule.head,rule.pos ++ guards,Set()) //assume that the conjunction of all guards is always needed (as opposed to e.g., one guard per rule etc)
    } else {
      rule
    }
  }

  private val VoidTick = Tick(Void,Void)

  private val __baseRules: Seq[AnnotatedNormalRule] = larsProgramEncoding.larsRuleEncodings map { encoding =>
    val rule = IncrementalAspPreparation.stripPositionAtoms(encoding.baseRule)
    val durationTick = encoding.ticksUntilBaseRuleIsIrrelevant
    if (durationTick == VoidTick) {
      StaticRule(rule)
    } else if (durationTick.count == Void) {
      RuleWithTimeDurationOnly(rule,durationTick,ExpirationOptional)
    } else if (durationTick.time == Void) {
      RuleWithCountDurationOnly(rule,durationTick,ExpirationOptional)
    } else {
      RuleWithDisjunctiveDuration(rule,durationTick,ExpirationOptional)
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

  val maxTimeWindowSize = larsProgramEncoding.windowAtomEncoders.collect{ case w:TimeWindowEncoder => w.size }.foldLeft(-1L)((m1,m2)=>Math.max(m1,m2))
  val maxTupleWindowSize = larsProgramEncoding.windowAtomEncoders.collect{ case w:TupleWindowEncoder => w.size }.foldLeft(-1L)((m1,m2)=>Math.max(m1,m2))
  val maxTupleBoxSize = larsProgramEncoding.windowAtomEncoders.collect{ case w:TupleBoxEncoder => w.size }.foldLeft(-1L)((m1,m2)=>Math.max(m1,m2))
  val maxTupleAtSize = larsProgramEncoding.windowAtomEncoders.collect{ case w:TupleAtEncoder => w.size }.foldLeft(-1L)((m1,m2)=>Math.max(m1,m2))

  val hasTupleWindow = maxTupleWindowSize > -1
  val need_tick_atoms = maxTupleBoxSize > -1
  val hasTupleBoxCombination = maxTupleBoxSize > -1
  val hasTupleAtCombination = maxTupleAtSize > -1
  val hasTimeWindow = maxTimeWindowSize > -1

//  val need_at_cnt_atoms = windowAtoms exists {
//    case WindowAtom(SlidingTupleWindow(_), _, _) => true
//    case _ => false
//  }

//  val need_tick_atoms = windowAtoms exists {
//    case WindowAtom(SlidingTupleWindow(_), Box, _) => true
//    case _ => false
//  }

}
