package engine.asp

import core._
import core.asp.{AspRule, NormalRule}
import core.lars._

import scala.concurrent.duration._

/**
  * Created by fm on 20/01/2017.
  */
case class PlainLarsToAspMapper(engineTimeUnit: EngineTimeUnit = 1 second) extends LarsToAspMapper {

  def identityRulesForAtom(a: Atom): Seq[NormalRule] = {
    Seq(
      AspRule[Atom, Atom](a, Set(now(TimePinVariable), PinnedAtom(a, TimePinVariable))),
      AspRule[Atom, Atom](PinnedAtom(a, TimePinVariable), Set(now(TimePinVariable), a))
    )
  }

  def encodingAtom(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    case AtAtom(t, a) => PinnedAtom(a, t)
    case a: Atom => a
    case a: WindowAtom => this.encodedWindowAtom(a)
  }

  // windowAtom: \window^1 @_T a(X)
  // head: w_{bla}(X,T)
  def slidingTime(window: SlidingTimeWindow, windowAtom: WindowAtom): WindowAtomEncoder = {
    val length = timePoints(window.windowSize.unit, window.windowSize.length)
    val head = encodedWindowAtom(windowAtom) //TODO beautify
    windowAtom.temporalModality match {
      case a: At => TimeAtEncoder(length, windowAtom.atom, head.asInstanceOf[PinnedAtAtom], a.time)
      case Diamond => TimeDiamondEncoder(length, windowAtom.atom, head)
      case Box => TimeBoxEncoder(length, windowAtom.atom, head)
    }
  }

  def slidingTuple(window: SlidingTupleWindow, windowAtom: WindowAtom): WindowAtomEncoder = {
    val head = encodedWindowAtom(windowAtom) //TODO beautify
    windowAtom.temporalModality match {
      case a: At => TupleAtEncoder(window.windowSize, windowAtom.atom, head.asInstanceOf[PinnedAtAtom], a.time)
      case Diamond => TupleDiamondEncoder(window.windowSize, windowAtom.atom, head)
      case Box => TupleBoxEncoder(window.windowSize, windowAtom.atom, head)
    }
  }

  def encodedWindowAtom(windowAtom: WindowAtom) = {
    val predicate = predicateFor(windowAtom)
    val previousArguments = windowAtom.atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }

    windowAtom.temporalModality match {
      case At(v: Time) => PinnedAtom.asPinnedAtAtom(Atom(predicate, previousArguments), v) //fine to say it is a w@(...)
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
   - for one-shot/reactive solving, everything is there by ruleEncodings plus the allWindowRules in windowAtomEncoders
   - for incremental solving, we use ruleEncodings + incrementalRulesAt (at every time point)

   b <- \window^1 \Diamond a

   b <- w           <-- this rule is contained in ruleEncodings
   w <- a(t-1)      <-- these rules are contained in allWindowRules, since they have a window atom representation in their head
   w <- a(t-0)
 */

case class TimeAtEncoder(length: Long, atom: Atom, windowAtomEncoding: PinnedAtAtom, atTime: Time) extends TimeWindowEncoder {

  val allWindowRules = (0 to length.toInt) map { i =>
    val timePos: Time = atTime match {
      case TimePoint(t) => t - i
      case _ => TimePinVariable - i
    }
    val head: PinnedAtAtom = windowAtomEncoding.resetPin(timePos)
    val b: Atom = PinnedAtom.asPinnedAtAtom(atom, timePos)
    AspRule[Atom,Atom](head, Set(now(TimePinVariable), b))
  }

  def rulesToGroundAt(tick: Tick): Seq[(Expiration,NormalRule)] = {
    val t = tick.time
    val head:Atom = windowAtomEncoding.resetPin(t)
    val posBody = Set(PinnedAtom.asPinnedAtAtom(atom,t))
    val rule: NormalRule = AspRule(head,posBody)
    val exp: Expiration = TickPair(t + length + 1, -1)
    Seq((exp,rule))
  }

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(length+1,-1)

}

/* EXAMPLE.
   b <- \window^range \Diamond a.
   ==>
   b <- w_{range-d-a}
   w_{range-d-a} <- now(N), a_at(T), T=N-0 //...range

   atom: Atom ... a
   windowAtomEncoding: w_{range-d-a}
 */
case class TimeDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TimeWindowEncoder with IncrementallyPinnedRules {

  val allWindowRules = (0 to length.toInt) map { i =>
    val b: Atom = PinnedAtom.asPinnedAtAtom(atom, TimePinVariable - i)
    AspRule[Atom, Atom](windowAtomEncoding, Set[Atom](now(TimePinVariable), b))
  }

  override def rulesToGroundAt(tick: Tick): Seq[(Expiration,NormalRule)] = {
    val t = tick.time
    val posBody = Set[Atom](PinnedAtom.asPinnedAtAtom(atom,t))
    val rule: NormalRule = AspRule(windowAtomEncoding,posBody)
    val exp: Expiration = TickPair(t + length + 1, -1)
    Seq((exp,rule))
  }

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1,-1) //since time variable not included

}

case class TimeBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TimeWindowEncoder with IncrementallyPinnedRules {

  val spoilerAtom = Atom(Predicate(f"spoil_te_${length}_${atom.predicate.caption}"), Atom.unapply(atom).getOrElse(Seq()))

  val baseRule: NormalRule = AspRule(windowAtomEncoding, Set(atom), Set(spoilerAtom))

  val spoilerRules: Seq[NormalRule] = (1 to length.toInt) map  { i =>
    AspRule(spoilerAtom, Set[Atom](atom, now(TimePinVariable)), Set[Atom](PinnedAtom.asPinnedAtAtom(atom, TimePinVariable - i)))
  }

  override val allWindowRules: Seq[NormalRule] = spoilerRules :+ baseRule

  override def rulesToGroundAt(tick: Tick): Seq[(Expiration,NormalRule)] = {
    val baseRule: NormalRule = AspRule(windowAtomEncoding,Set(atom),Set(spoilerAtom))
    val expBase: Expiration = TickPair(-1,-1)
    if (length == 0) return Seq((expBase,baseRule))

    val spoilerRule: NormalRule = AspRule(spoilerAtom, Set(atom, now(TimePinVariable)), Set(PinnedAtom.asPinnedAtAtom(atom, TimePinVariable - 1)))
    val expSp: Expiration = TickPair(tick.time + length, -1)
    Seq((expBase,baseRule),(expSp,spoilerRule))
  }

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1,-1) //since time variable not included
}

case class TupleAtEncoder(length: Long, atom: Atom, windowAtomEncoding: PinnedAtAtom, atTime: Time) extends TupleWindowEncoder with IncrementallyPinnedRules {
  val D = Variable("DD")

  // at atoms got their parameter already encoded
  val allWindowRules = (0 to length.toInt) map { i =>
    AspRule[Atom, Atom](windowAtomEncoding, Set(cnt(CountPinVariable), PinnedAtom.asPinnedAtCntAtom(atom, atTime, D), Sum(CountPinVariable, IntValue(-i), D)))
  }

  override def rulesToGroundAt(tick: Tick): Seq[(Expiration,NormalRule)] = {
    val c = tick.count.toInt
    val head:Atom = windowAtomEncoding.asInstanceOf[Atom]
    val posBody = Set(PinnedAtom.asPinnedAtCntAtom(atom,atTime,IntValue(c)))
    val rule: NormalRule = AspRule(head,posBody)
    val exp: Expiration = TickPair(-1, c + length)
    Seq((exp,rule))
  }

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1, length)
}

case class TupleDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TupleWindowEncoder with IncrementallyPinnedRules {

  val allWindowRules = (0 to length.toInt) map { i =>
    AspRule(windowAtomEncoding, Set(cnt(CountPinVariable), PinnedAtom.asPinnedCntAtom(atom, CountPinVariable - i)))
  }

  override def rulesToGroundAt(tick: Tick): Seq[(Expiration,NormalRule)] = {
    val c = tick.count.toInt
    val posBody = Set(PinnedAtom.asPinnedCntAtom(atom,IntValue(c)))
    val rule: NormalRule = AspRule(windowAtomEncoding,posBody)
    val exp: Expiration = TickPair(-1, c + length)
    Seq((exp,rule))
  }

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1,-1) //no time/count variable in window atom
}

case class TupleBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TupleWindowEncoder with IncrementallyPinnedRules {

  val spoilerAtom = Atom(Predicate(f"spoil_tu_${length}_${atom.predicate.caption}"), Atom.unapply(atom).getOrElse(Seq()))

  val baseRule: NormalRule = AspRule(windowAtomEncoding, Set(atom), Set(spoilerAtom))

  val spoilerRules1: Seq[NormalRule] = (1 to length.toInt-1) map { i =>
    AspRule(spoilerAtom,
      Set[Atom](atom,cnt(CountPinVariable)),
      Set[Atom](PinnedAtom.asPinnedCntAtom(atom, CountPinVariable - i)))
  }

  val T1 = Variable("TT1")
  val T2 = Variable("TT2")

  val spoilerRules2: Seq[NormalRule] = (1 to length.toInt-1) map { i =>
    AspRule(spoilerAtom,
      Set[Atom](
        atom,
        cnt(CountPinVariable),
        PinnedAtom.asPinnedAtCntAtom(atom, T1, CountPinVariable - i),
        PinnedAtom.asPinnedAtCntAtom(atom, T2, CountPinVariable - i + 1),
        Gt(T2, T1 + 1)
      ))
  }

  val spoilerRules: Seq[NormalRule] = spoilerRules1 ++ spoilerRules2

  override val allWindowRules: Seq[NormalRule] = spoilerRules :+ baseRule

  override def rulesToGroundAt(tick: Tick): Seq[(Expiration,NormalRule)] = {
    val t = TimePoint(tick.time)
    val c = IntValue(tick.count.toInt)

    val baseRule: NormalRule = AspRule(windowAtomEncoding,Set(atom),Set(spoilerAtom))
    val expBase: Expiration = TickPair(-1,-1)
    if (length < 2) return Seq((expBase,baseRule))

    val spoilerRule1: NormalRule = AspRule(spoilerAtom,
      Set[Atom](atom),
      Set[Atom](PinnedAtom.asPinnedCntAtom(atom, c - 1)))
    val spoilerRule2: NormalRule = AspRule(spoilerAtom,
      Set[Atom](
        atom,
        PinnedAtom.asPinnedAtCntAtom(atom, T1, c - 1),
        PinnedAtom.asPinnedAtCntAtom(atom, t, c),
        Gt(t, T1 + 1)
      ))

    val expSp: Expiration = TickPair(-1, tick.count + length - 1)
    Seq((expBase,baseRule),(expSp,spoilerRule1),(expSp,spoilerRule2))
  }

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1, -1) //no time/count variable in window atom
}

