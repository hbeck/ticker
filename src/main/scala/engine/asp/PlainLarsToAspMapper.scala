package engine.asp

import core._
import core.asp.{AspRule, NormalRule, UserDefinedAspRule}
import core.lars._
import engine.asp.tms._

import scala.concurrent.duration._

/**
  * Created by fm on 20/01/2017.
  */
case class PlainLarsToAspMapper(engineTimeUnit: EngineTimeUnit = 1 second) extends LarsToAspMapper {

  override def identityRulesForAtom(a: Atom): Seq[NormalRule] = {
    Seq(
      AspRule[Atom, Atom](a, Set(now(TimePinVariable), PinnedAtom.asPinnedAtAtom(a, TimePinVariable))),
      AspRule[Atom, Atom](PinnedAtom.asPinnedAtAtom(a, TimePinVariable), Set(now(TimePinVariable), a))
    )
  }

  def encodingAtom(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    case AtAtom(t, a) => PinnedAtom.asPinnedAtAtom(a, t)
    case a: Atom => a
    case a: WindowAtom => encodedWindowAtom(a)
  }

  // windowAtom: \window^1 @_T a(X)
  // head: w_{bla}(X,T)
  override def slidingTime(window: SlidingTimeWindow, windowAtom: WindowAtom, groundingGuards: Set[Atom]=Set()): WindowAtomEncoder = {
    val length = timePoints(window.windowSize.unit, window.windowSize.length)
    val head = encodedWindowAtom(windowAtom) //may be improved
    windowAtom.temporalModality match {
      case a: At => TimeAtEncoder(length, windowAtom.atom, head.asInstanceOf[PinnedAtAtom], a.time, groundingGuards)
      case Diamond => TimeDiamondEncoder(length, windowAtom.atom, head, groundingGuards)
      case Box => TimeBoxEncoder(length, windowAtom.atom, head, groundingGuards)
    }
  }

  override def slidingTuple(window: SlidingTupleWindow, windowAtom: WindowAtom, groundingGuards: Set[Atom]=Set()): WindowAtomEncoder = {
    val head = encodedWindowAtom(windowAtom) //may be improved
    windowAtom.temporalModality match {
      case a: At => TupleAtEncoder(window.windowSize, windowAtom.atom, head.asInstanceOf[PinnedAtAtom], a.time, groundingGuards)
      case Diamond => TupleDiamondEncoder(window.windowSize, windowAtom.atom, head, groundingGuards)
      case Box => TupleBoxEncoder(window.windowSize, windowAtom.atom, head, groundingGuards)
    }
  }

  def encodedWindowAtom(windowAtom: WindowAtom) = {
    val predicate = predicateFor(windowAtom)
    val previousArguments = windowAtom.atom match {
      case aa: AtomWithArguments => aa.arguments
      case a: Atom => Seq()
    }

    windowAtom.temporalModality match {
      case At(v: Time) => PinnedAtom.asPinnedAtAtom(Atom(predicate, previousArguments), v) //note the slight difference in semantics: if this pinned atom holds, then the atom within holds at the respective time, not the window atom itself
      case _ => Atom(predicate, previousArguments)
    }
  }

  def timePoints(unit: TimeUnit, size: Long) = Duration(unit.toMillis(size) / engineTimeUnit.toMillis, engineTimeUnit.unit).length
}


object PlainLarsToAspMapper {
  def asNormalRule(rule: Rule[Atom, Atom]): NormalRule = AspRule(rule.head, rule.pos, rule.neg)

  def asNormalRules(rule: Rule[Atom, Atom]): Seq[NormalRule] = Seq(asNormalRule(rule))

  def asNormalRules(rules: Seq[Rule[Atom, Atom]]): Seq[NormalRule] = rules map asNormalRule
}

/*
   at this point we have a representation for multiple evaluation modes:
   - for one-shot solving, everything is there by ruleEncodings plus the allWindowRules in windowAtomEncoders
   - for incremental solving, we use ruleEncodings + incrementalRulesAt (at every time point)

   b <- \window^1 \Diamond a

   b <- w           <-- this rule is contained in ruleEncodings
   w <- a(t-1)      <-- these rules are contained in allWindowRules, since they have a window atom representation in their head
   w <- a(t-0)
 */

case class TimeAtEncoder(length: Long, atom: Atom, windowAtomEncoding: PinnedAtAtom, atTime: Time, groundingGuards: Set[Atom]) extends TimeWindowEncoder {

  override lazy val allWindowRules = (0 to length.toInt) map { i =>
    val timePos: Time = atTime match {
      case TimePoint(t) => t - i
      case _ => TimePinVariable - i
    }
    val head: PinnedAtAtom = windowAtomEncoding.resetPin(timePos)
    val b: Atom = PinnedAtom.asPinnedAtAtom(atom, timePos)
    AspRule[Atom,Atom](head, Set(now(TimePinVariable), b))
  }

  override val windowRuleTemplates: Seq[AnnotatedNormalRule] = {
    val posBody = Set[Atom](PinnedAtom.asPinnedAtAtom(atom,atTime)) ++ groundingGuards
    val rule: NormalRule = UserDefinedAspRule(windowAtomEncoding,posBody,Set())
    val exp: TickDuration =  Tick(length + 1, Void)
    Seq(RuleWithTimeDurationOnly(rule,exp,ExpirationObligatory))
  }

  override def ticksUntilWindowAtomIsOutdated(): TickDuration = Tick(length+1,Void)

}

/* EXAMPLE.
   b <- \window^range \Diamond a.
   ==>
   b <- w_{range-d-a}
   w_{range-d-a} <- now(N), a_at(T), T=N-0 //...range

   atom: Atom ... a
   windowAtomEncoding: w_{range-d-a}
 */
case class TimeDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, groundingGuards: Set[Atom]) extends TimeWindowEncoder {

  override lazy val allWindowRules = (0 to length.toInt) map { i =>
    val b: Atom = PinnedAtom.asPinnedAtAtom(atom, TimePinVariable - i)
    AspRule[Atom, Atom](windowAtomEncoding, Set[Atom](now(TimePinVariable), b))
  }

  override val windowRuleTemplates: Seq[AnnotatedNormalRule] = {
    val posBody = Set[Atom](PinnedAtom.asPinnedAtAtom(atom,TimePinVariable)) ++ groundingGuards
    val rule: NormalRule = AspRule(windowAtomEncoding,posBody)
    val exp: TickDuration =  Tick(length+1,Void)
    Seq(RuleWithTimeDurationOnly(rule,exp,ExpirationObligatory))
  }

  override def ticksUntilWindowAtomIsOutdated(): TickDuration = Tick(Void,Void) //since time variable not included

}

case class TimeBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, groundingGuards: Set[Atom]) extends TimeWindowEncoder {

  val spoilerAtom = Atom(Predicate(f"spoil_te_${length}_${atom.predicate.caption}"), Atom.unapply(atom).getOrElse(Seq()))

  val staticRule: NormalRule = AspRule(windowAtomEncoding, Set(atom), Set(spoilerAtom))

  val zero = TimePoint(0)

  lazy val spoilerRules: Seq[NormalRule] = (1 to length.toInt) map  { i =>
    AspRule(spoilerAtom, Set[Atom](atom, now(TimePinVariable), Geq(TimePinVariable-i,zero)), Set[Atom](PinnedAtom.asPinnedAtAtom(atom, TimePinVariable - i)))
  }

  override lazy val allWindowRules: Seq[NormalRule] = spoilerRules :+ staticRule

  override val windowRuleTemplates: Seq[AnnotatedNormalRule] = {
    val staticRule: NormalRule = AspRule(windowAtomEncoding, Set(atom)++groundingGuards, Set(spoilerAtom))
    if (length == 0) {
      Seq(StaticRule(staticRule))
    } else {
      val spoilerRule: NormalRule = AspRule(spoilerAtom, Set(atom,Geq(TimeVariableWithOffset(TimePinVariable,-1),zero))++groundingGuards, Set(PinnedAtom.asPinnedAtAtom(atom, TimeVariableWithOffset(TimePinVariable,-1))))
      val expSp: TickDuration =  Tick(length,Void)
      Seq(StaticRule(staticRule),RuleWithTimeDurationOnly(spoilerRule,expSp,ExpirationObligatory))
    }
  }

  override def ticksUntilWindowAtomIsOutdated(): TickDuration = Tick(Void,Void) //since time variable not included
}

case class TupleAtEncoder(length: Long, atom: Atom, windowAtomEncoding: PinnedAtAtom, atTime: Time, groundingGuards: Set[Atom]) extends TupleWindowEncoder {
  val D = Variable("DD")

  // at-atoms got their parameter already encoded
  override lazy val allWindowRules = (0 to length.toInt) map { i =>
    AspRule[Atom, Atom](windowAtomEncoding, Set(cnt(CountPinVariable), PinnedAtom.asPinnedAtCntAtom(atom, atTime, D), Plus(CountPinVariable, IntValue(-i), D)))
  }

  override val windowRuleTemplates: Seq[AnnotatedNormalRule] = {
    val posBody = Set(PinnedAtom.asPinnedAtCntAtom(atom,atTime,CountPinVariable))++groundingGuards
    val rule: NormalRule = UserDefinedAspRule(windowAtomEncoding,posBody,Set())
    val exp: TickDuration =  Tick(Void,length)
    Seq(RuleWithCountDurationOnly(rule,exp,ExpirationObligatory))
  }

  override def ticksUntilWindowAtomIsOutdated(): TickDuration = Tick(Void, length)
}

case class TupleDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, groundingGuards: Set[Atom]) extends TupleWindowEncoder {

  val T = TimeVariableWithOffset(StringVariable("TT"))

  lazy val allWindowRules = (0 to length.toInt) map { i =>
    AspRule(windowAtomEncoding, Set(cnt(CountPinVariable), PinnedAtom.asPinnedAtCntAtom(atom, T, CountPinVariable - i)))
  }

  override val windowRuleTemplates: Seq[AnnotatedNormalRule] = {
    val posBody = Set(PinnedAtom.asPinnedAtCntAtom(atom,TimePinVariable,CountPinVariable))++groundingGuards
    val rule: NormalRule = AspRule(windowAtomEncoding,posBody)
    val exp: TickDuration =  Tick(Void,length)
    Seq(RuleWithCountDurationOnly(rule,exp,ExpirationObligatory))
  }

  override def ticksUntilWindowAtomIsOutdated(): TickDuration = Tick(Void,Void) //no time/count variable in window atom
}

case class TupleBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, groundingGuards: Set[Atom]) extends TupleWindowEncoder {

  val spoilerAtom = Atom(Predicate(f"spoil_tu_${length}_${atom.predicate.caption}"), Atom.unapply(atom).getOrElse(Seq()))

  val staticRule: NormalRule = AspRule(windowAtomEncoding, Set(atom), Set(spoilerAtom))

  val T = TimeVariableWithOffset("TT")
  val D = IntVariableWithOffset("DD")
  val D2 = IntVariableWithOffset("DD2")

  lazy val spoilerRuleCoverTime: NormalRule = AspRule(spoilerAtom,
    Set[Atom](atom,cnt(CountPinVariable),tickAtom(T,D), Leq(CountPinVariable - length.toInt + 1,D), Leq(D,CountPinVariable)),
    Set[Atom](PinnedAtom.asPinnedAtAtom(atom, T)))

  lazy val spoilerRuleCoverCount: NormalRule = AspRule(spoilerAtom,
    Set[Atom](atom,cnt(CountPinVariable),tickAtom(T,D),
      Eq(D,CountPinVariable - length.toInt + 1), PinnedAtom.asPinnedAtCntAtom(atom,T,D2), Lt(D2,D)),
    Set[Atom]())

  lazy val spoilingRules: Seq[NormalRule] = Seq(spoilerRuleCoverTime,spoilerRuleCoverCount)

  override lazy val allWindowRules: Seq[NormalRule] = spoilingRules :+ staticRule

  //this variable may go if we can remove predicate "tick" from the pos body of the incremental rule
  //requires more intelligent grounding

  override val windowRuleTemplates: Seq[AnnotatedNormalRule] = {

    val staticRule: NormalRule = AspRule(windowAtomEncoding, Set(atom)++groundingGuards, Set(spoilerAtom))

    if (length < 2) {
      Seq(StaticRule(staticRule))
    } else {

      val coversTime = Atom(Predicate(f"covT_${length}"),Seq(TimePinVariable))
      val coversCount = Atom(Predicate(f"covC_${length}"),Seq(CountPinVariable))
      val tickWithVars = tickAtom(TimePinVariable,CountPinVariable)

      val spoilerRuleCoverTime: NormalRule = AspRule(spoilerAtom,
        Set[Atom](atom, tickAtom(TimePinVariable, CountPinVariable), coversTime) ++ groundingGuards,
        Set[Atom](PinnedAtom.asPinnedAtAtom(atom, TimePinVariable)))

      val spoilerRuleCoverCount: NormalRule = AspRule(spoilerAtom,
        Set[Atom](atom, PinnedAtom.asPinnedAtCntAtom(atom, TimePinVariable, CountPinVariable), coversTime) ++ groundingGuards,
        Set[Atom](coversCount))

      val expSpoilerCoverTime: TickDuration = Tick(Void, length)
      val expSpoilerCoverCount: TickDuration = Tick(length, Void)

      val expCovers: TickDuration = Tick(Void, length)
      val ruleDurCoverT = RuleWithCountDurationOnly(AspRule(coversTime, tickWithVars), expCovers, ExpirationObligatory, OnTimeAndCountIncrease)
      val ruleDurCoverC = RuleWithCountDurationOnly(AspRule(coversCount, tickWithVars), expCovers, ExpirationObligatory, OnTimeAndCountIncrease)

      Seq(StaticRule(staticRule),
        RuleWithCountDurationOnly(spoilerRuleCoverTime, expSpoilerCoverTime, ExpirationObligatory, OnTimeAndCountIncrease),
        RuleWithTimeDurationOnly(spoilerRuleCoverCount, expSpoilerCoverCount, ExpirationOptional, OnTimeAndCountIncrease),
        ruleDurCoverT, ruleDurCoverC)
    }
  }

  override def ticksUntilWindowAtomIsOutdated(): TickDuration = Tick(Void, Void) //no time/count variable in window atom
}
