package engine.asp

import core._
import core.asp.{AspFact, AspRule, NormalProgram, NormalRule}
import core.lars.{TimeUnit => _, TimePinVariable => _, _}

import scala.concurrent.duration._

import PlainLarsToReactiveMapper.{t, c}

object PlainLarsToReactiveMapper {
  val t = TimeVariableWithOffset(Variable("t"))
  val c = Variable("c")
}

/**
  * Created by fm on 20/01/2017.
  */
case class PlainLarsToReactiveMapper(engineTimeUnit: EngineTimeUnit = 1 second) extends LarsToAspMapper {

  def identityRulesForAtom(a: Atom): Seq[NormalRule] = {
    Seq(
      //      AspRule[Atom, Atom](a(T), Set(now.apply(T), PinnedAtom(a, T))),
      //      AspRule[Atom, Atom](PinnedAtom(a, T), Set(now(T), a(T)))
    )
  }

  override def encode(rule: LarsRule): NormalRule = {
    AspRule(
      encodeHeadAtom(rule.head),
      (rule.pos map this.encodingAtom) + now(t) + cnt(c),
      rule.neg map this.encodingAtom
    )
  }

  def encodeHeadAtom(headAtom: HeadAtom): Atom = headAtom match {
    //    case AtAtom(_T, a) => PinnedAtom(a.appendTimeAsNormalArgument(_T), t)
    case AtAtom(_T, a) => PinnedAtom(a, _T)
    //    case AtAtom(_T: TimeVariableWithOffset, a) => PinnedAtom(a, TimeVariableWithOffset(t.variable, _T.offset))
    case a: Atom => PinnedAtom(a, t)
  }


  def encodingAtom(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    //    case AtAtom(_T, a) => PinnedAtom(a.appendTimeAsNormalArgument(_T), t)
    //    case AtAtom(_T: TimeVariableWithOffset, a) => PinnedAtom(a, TimeVariableWithOffset(t.variable, _T.offset))
    case AtAtom(_T, a) => PinnedAtom(a, _T)
    case a: WindowAtom => this.encodedWindowAtom(a)
    case a: Atom => PinnedAtom(a, t)
  }

  // \window^1 @_T a(X)
  // head: w_{bla}(X,T)
  def slidingTime(window: SlidingTimeWindow, windowAtom: WindowAtom): WindowAtomEncoder = {
    val length = timePoints(window.windowSize.unit, window.windowSize.length)
    val head = encodedWindowAtom(windowAtom) //TODO beautify
    windowAtom.temporalModality match {
      case a: At => ReactiveTimeAtEncoder(length, windowAtom.atom, head, a.time)
      case Diamond => ReactiveTimeDiamondEncoder(length, windowAtom.atom, head)
      case Box => ReactiveTimeBoxEncoder(length, windowAtom.atom, head)
    }
  }

  def slidingTuple(window: SlidingTupleWindow, windowAtom: WindowAtom): WindowAtomEncoder = {
    val head = encodedWindowAtom(windowAtom) //TODO beautify
    windowAtom.temporalModality match {
      case Diamond => ReactiveTupleDiamondEncoder(window.windowSize, windowAtom.atom, head)
      case Box => ReactiveTupleBoxEncoder(window.windowSize, windowAtom.atom, head)
      case a: At => ReactiveTupleAtEncoder(window.windowSize, windowAtom.atom, head, a.time)
    }
  }

  def encodedWindowAtom(windowAtom: WindowAtom) = {
    val predicate = predicateFor(windowAtom)
    val previousArguments = windowAtom.atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }

    windowAtom.temporalModality match {
      case At(_T) => Atom(predicate, previousArguments).apply(_T, tick(windowAtom))
      case _ => PinnedAtom(Atom(predicate, previousArguments), tick(windowAtom))
    }
  }

  def tick(windowAtom: WindowAtom) = windowAtom.windowFunction match {
    case SlidingTimeWindow(_) => t
    case SlidingTupleWindow(_) => c
  }

  def timePoints(unit: TimeUnit, size: Long) = Duration(unit.toMillis(size) / engineTimeUnit.toMillis, engineTimeUnit.unit).length

}


case class ReactiveTimeAtEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, time: Time = t) extends TimeWindowEncoder {

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1,-1) //???

  val N = TimeVariableWithOffset("N") //TODO hb review N is not necessarily a time variable! --> distinction between time variable and other useful?
  //TODO if we want a distinction between arbitrary variables and those with an offset, it should rather be IntVariable (which then always implicitly
  //allows the use of an offset).


  val parameter = time match {
    case tp: TimePoint => tp
    case _ => t // we want T as parameter so pinning is easy later on
  }

  // we need to unpack the windowAtomEncoding (from the PinnedAtom) in order to create a PinnedAtom(atom, T-k)
  //  private val unpackedWindowAtom = windowAtomEncoding.atom
  private val windowAtomArguments = Atom.unapply(windowAtomEncoding).getOrElse(Seq())
  private val unpackedWindowAtom = Atom(windowAtomEncoding.predicate, windowAtomArguments.dropRight(2))

  val rule: NormalRule = AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, time), Set[Atom](now(N), PinnedAtom(atom, time)))
  val allWindowRules = (0 to length.toInt) map { i =>
    AspRule[Atom, Atom](unpackedWindowAtom.apply(parameter - i, parameter), Set[Atom](now(parameter), PinnedAtom(atom, parameter - i)))
  }

  def incrementalRulesAt(currentPosition: TickPosition): IncrementalRules = {
//    val added = rule.assign(Assignment(Map(t -> i, N -> i)))
//    val removed = rule.assign(Assignment(Map(t -> IntValue(i.int - length.toInt), N -> i)))
//
//    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
    null
  }
}

/* EXAMPLE.
   b <- \window^range \Diamond a.
   ==>
   b <- w_{range-d-a}
   w_{range-d-a} <- now(N), a_at(T), T=N-0 //...range

   atom: Atom ... a
   windowAtomEncoding: w_{range-d-a}
 */
case class ReactiveTimeDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TimeWindowEncoder {
  val N: Variable = TimeVariableWithOffset("N")

  val rule: NormalRule = AspRule(windowAtomEncoding, Set[Atom](now(N), PinnedAtom(atom, t)))
  val allWindowRules = (0 to length.toInt) map (i => AspRule(windowAtomEncoding, Set[Atom](now(t), PinnedAtom(atom, t - i))))

  //TODO hb prepared for later use
  def incrementalRulesAt(currentPosition: TickPosition): IncrementalRules = {
//    val added = rule.assign(Assignment(Map(t -> i, N -> i)))
//    val removed = rule.assign(Assignment(Map(t -> IntValue(i.int - length.toInt), N -> i)))
//
//    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
    null
  }

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1,-1) //???
}


case class ReactiveTimeBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TimeWindowEncoder {

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1,-1) //???

  val N: Variable = TimeVariableWithOffset("N")
  val spoilerPredicate = Predicate(f"spoil_ti_${length}_${atom.predicate.caption}")

  val spoilerAtom = PinnedAtom(Atom(spoilerPredicate, Atom.unapply(atom).getOrElse(Seq())), t)

  val baseRule: NormalRule = AspRule(windowAtomEncoding, Set(PinnedAtom(atom, t)), Set(spoilerAtom))
  val spoilerRules: Seq[NormalRule] = (1 to length.toInt) map (i => AspRule(spoilerAtom, Set[Atom](PinnedAtom(atom, t), now(t)), Set[Atom](PinnedAtom(atom, t - i))))

  override val allWindowRules: Seq[NormalRule] = spoilerRules :+ baseRule


  val incrementalRule: NormalRule = AspRule(spoilerAtom, Set[Atom](atom, now(N)), Set[Atom](PinnedAtom(atom, t)))

  def incrementalRulesAt(currentPosition: TickPosition): IncrementalRules = {
//    val added = incrementalRule.assign(Assignment(Map(N -> tick, t -> tick)))
//    val removed = incrementalRule.assign(Assignment(Map(N -> tick, t -> IntValue(tick.int - length.toInt))))

    // TODO: base rule is added every time - shouldn't matter because of set-semantics...
    //    IncrementalRules(added + baseRule, removed)
    null
  }
}

case class ReactiveTupleDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TupleWindowEncoder {

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1,-1) //???

  val C = TimeVariableWithOffset("C")
  //TODO hb review why time variable?
  val D = Variable("D") //TODO ... and then why this not?

  val rule: NormalRule = AspRule(windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom(atom, D)))

  val allWindowRules = (0 to length.toInt) map (i => AspRule(windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom.asPinnedCntAtom(atom, C - i))))


  def incrementalRulesAt(currentPosition: TickPosition): IncrementalRules = {
//    val added = rule.assign(Assignment(Map(D -> i, C -> i)))
//    val removed = rule.assign(Assignment(Map(D -> IntValue(i.int - length.toInt), C -> i)))
//
//    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
    null
  }
}


case class ReactiveTupleBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TupleWindowEncoder {

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1,-1) //???

  val C: Variable = TimeVariableWithOffset("C")
  val D: Variable = TimeVariableWithOffset("D")

  val D1: Variable = TimeVariableWithOffset("D1")
  val D2: Variable = TimeVariableWithOffset("D2")

  val T1: Variable = TimeVariableWithOffset("T1")
  val T2: Variable = TimeVariableWithOffset("T2")

  val C_diff: Variable = TimeVariableWithOffset("C_diff")
  val T_plus1: Variable = TimeVariableWithOffset("T_plus1")
  val D_plus1: Variable = TimeVariableWithOffset("D_plus1")

  val spoilerPredicate = Predicate(f"spoil_tu_${length}_${atom.predicate.caption}")
  val spoilerAtom = PinnedAtom(Atom(spoilerPredicate, Atom.unapply(atom).getOrElse(Seq())), t)

  val baseRule: NormalRule = AspRule(windowAtomEncoding, Set(PinnedAtom(atom, t)), Set(spoilerAtom))
  val cntSpoilerRules_1: Seq[NormalRule] = (1 to length.toInt) map { i =>
    AspRule(
      spoilerAtom,
      Set[Atom](
        PinnedAtom(atom, t),
        cnt(C),
        Sum(C, IntValue(-(i)), D)
      ),
      Set[Atom](
        PinnedAtom.asPinnedCntAtom(atom, D)
      )
    )
  }

  val cntSpoilerRules_2: NormalRule = AspRule[Atom, Atom](
    spoilerAtom,
    Set[Atom](
      PinnedAtom(atom, t),
      cnt(C),
      PinnedAtom(atom, T1, D1),
      PinnedAtom(atom, T2, D2),
      Sum(C, IntValue(-length.toInt), C_diff),
      Geq(D1, C_diff),
      Sum(D1, IntValue(1), D_plus1),
      Eq(D_plus1, D2),
      Sum(T1, IntValue(1), T_plus1),
      Gt(T2, T_plus1)
    )
  )

  val spoilerRules: Seq[NormalRule] = cntSpoilerRules_1 :+ cntSpoilerRules_2

  override val allWindowRules: Seq[NormalRule] = spoilerRules :+ baseRule

  val incrementalRule: NormalRule = AspRule(spoilerAtom, Set[Atom](atom, now(C)), Set[Atom](PinnedAtom(atom, t)))

  def incrementalRulesAt(currentPosition: TickPosition): IncrementalRules = {
//    val added = incrementalRule.assign(Assignment(Map(C -> tick, t -> tick)))
//    val removed = incrementalRule.assign(Assignment(Map(C -> tick, t -> IntValue(tick.int - length.toInt))))

    // TODO: base rule is added every time - shouldn't matter because of set-semantics...
    //    IncrementalRules(added + baseRule, removed)
    null
  }
}

case class ReactiveTupleAtEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, timeVariable: Time = t) extends TupleWindowEncoder {

  override def ticksUntilWindowAtomIsOutdated(): TicksUntilOutdated = TickPair(-1,-1) //???

  val D = TimeVariableWithOffset("D")
  val C = TimeVariableWithOffset("C")

  val rule: NormalRule = AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, t), Set[Atom](now(C), PinnedAtom(atom, timeVariable)))

  // at atoms got their parameter already encoded
  val allWindowRules = (0 to length.toInt) map (i => AspRule[Atom, Atom](windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom(atom, timeVariable, D), Sum(D, IntValue(-i), D))))

  def incrementalRulesAt(currentPosition: TickPosition): IncrementalRules = {
    null
  }
}