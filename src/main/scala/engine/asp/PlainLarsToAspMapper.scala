package engine.asp

import core._
import core.asp.{AspFact, AspRule, NormalProgram, NormalRule}
import core.lars._

import scala.concurrent.duration._



/**
  * Created by fm on 20/01/2017.
  */
case class PlainLarsToAspMapper(engineTimeUnit: EngineTimeUnit = 1 second) extends LarsToAspMapper {

  def identityRulesForAtom(a: Atom): Seq[NormalRule] = {
    Seq(
      AspRule[Atom, Atom](a, Set(now.apply(T), PinnedAtom(a, T))),
      AspRule[Atom, Atom](PinnedAtom(a, T), Set(now(T), a))
    )
  }

  def encodingAtom(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    case AtAtom(t, a) => PinnedAtom(a, t)
    case a: Atom => a
    case a: WindowAtom => this.encodedWindowAtom(a)
  }

  // \window^1 @_T a(X)
  // head: w_{bla}(X,T)
  def slidingTime(window: SlidingTimeWindow, windowAtom: WindowAtom): WindowAtomEncoder = {
    val length = timePoints(window.windowSize.unit, window.windowSize.length)
    val head = encodedWindowAtom(windowAtom) //TODO beautify
    windowAtom.temporalModality match {
      case a: At => TimeAtEncoder(length, windowAtom.atom, head, a.time)
      case Diamond => TimeDiamondEncoder(length, windowAtom.atom, head)
      case Box => TimeBoxEncoder(length, windowAtom.atom, head)
    }
  }

  def slidingTuple(window: SlidingTupleWindow, windowAtom: WindowAtom): WindowAtomEncoder = {
    val head = encodedWindowAtom(windowAtom) //TODO beautify
    windowAtom.temporalModality match {
      case Diamond => TupleDiamondEncoder(window.windowSize, windowAtom.atom, head)
      case Box => TupleBoxEncoder(window.windowSize, windowAtom.atom, head)
      case a: At => TupleAtEncoder(window.windowSize, windowAtom.atom, head, a.time)
    }
  }

  def encodedWindowAtom(windowAtom: WindowAtom) = {
    val predicate = predicateFor(windowAtom)
    val previousArguments = windowAtom.atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }

    windowAtom.temporalModality match {
      case At(v: Time) => PinnedAtom(Atom(predicate, previousArguments), v)
      case _ => Atom(predicate, previousArguments)
    }
  }

  def timePoints(unit: TimeUnit, size: Long) = Duration(unit.toMillis(size) / engineTimeUnit.toMillis, engineTimeUnit.unit).length

}

//TODO hb review naming
/*
   at this point we have a representation for multiple evaluation modes:
   - for one-shot/reactive solving, everything is there by ruleEncodings plus the allWindowRules in windowAtomEncoders
   - for incremental solving, we use ruleEncodings + incrementalRulesAt (at every time point)

   b <- \window^1 \Diamond a

   b <- w           <-- this rule is contained in ruleEncodings
   w <- a(t-1)      <-- these rules are contained in allWindowRules, since they have a window atom representation in their head
   w <- a(0)
 */

case class TimeAtEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, time: Time = T) extends TimeWindowEncoder {
  val N = TimeVariableWithOffset("N") //TODO hb review N is not necessarily a time variable! --> distinction between time variable and other useful?
  //TODO if we want a distinction between arbitrary variables and those with an offset, it should rather be IntVariable (which then always implicitly
  //allows the use of an offset).


  val parameter = time match {
    case tp: TimePoint => tp
    case _ => T // we want T as parameter so pinning is easy later on
  }

  // we need to unpack the windowAtomEndocding (from the PinnedAtom) in order to create a PinnedAtom(atom, T-k)
  private val unpackedWindowAtom = windowAtomEncoding.atom

  val rule: NormalRule = AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, time), Set[Atom](now(N), PinnedAtom(atom, time)))
  val allWindowRules = (0 to length.toInt) map (i => AspRule[Atom, Atom](PinnedAtom(unpackedWindowAtom, parameter - i), Set[Atom](now(parameter), PinnedAtom(atom, parameter - i))))

  override def incrementalRulesAt(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(T -> i, N -> i)))
    val removed = rule.assign(Assignment(Map(T -> IntValue(i.int - length.toInt), N -> i)))

    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
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
case class TimeDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TimeWindowEncoder {
  val N: Variable = TimeVariableWithOffset("N")

  val rule: NormalRule = AspRule(windowAtomEncoding, Set[Atom](now(N), PinnedAtom(atom, T)))
  val allWindowRules = (0 to length.toInt) map (i => AspRule(windowAtomEncoding, Set[Atom](now(T), PinnedAtom(atom, T - i))))

  //TODO hb prepared for later use
  override def incrementalRulesAt(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(T -> i, N -> i)))
    val removed = rule.assign(Assignment(Map(T -> IntValue(i.int - length.toInt), N -> i)))

    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
  }
}


case class TimeBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TimeWindowEncoder {

  val N: Variable = TimeVariableWithOffset("N")

  val spoilerAtom = Atom(Predicate(f"spoil_ti_${length}_${atom.predicate.caption}"), Atom.unapply(atom).getOrElse(Seq()))

  val baseRule: NormalRule = AspRule(windowAtomEncoding, Set(atom), Set(spoilerAtom))
  val spoilerRules: Seq[NormalRule] = (1 to length.toInt) map (i => AspRule(spoilerAtom, Set[Atom](atom, now(T)), Set[Atom](PinnedAtom(atom, T - i))))

  override val allWindowRules: Seq[NormalRule] = spoilerRules :+ baseRule


  val incrementalRule: NormalRule = AspRule(spoilerAtom, Set[Atom](atom, now(N)), Set[Atom](PinnedAtom(atom, T)))

  override def incrementalRulesAt(tick: IntValue): IncrementalRules = {
    val added = incrementalRule.assign(Assignment(Map(N -> tick, T -> tick)))
    val removed = incrementalRule.assign(Assignment(Map(N -> tick, T -> IntValue(tick.int - length.toInt))))

    // TODO: base rule is added every time - shouldn't matter because of set-semantics...
    //    IncrementalRules(added + baseRule, removed)
    null
  }
}

case class TupleDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TupleWindowEncoder {
  val C = TimeVariableWithOffset("C")
  //TODO hb review why time variable?
  val D = Variable("D") //TODO ... and then why this not?

  val rule: NormalRule = AspRule(windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom(atom, D)))

  val allWindowRules = (0 to length.toInt) map (i => AspRule(windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom.asCount(atom, D), Sum(C, IntValue(-i), D))))


  override def incrementalRulesAt(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(D -> i, C -> i)))
    val removed = rule.assign(Assignment(Map(D -> IntValue(i.int - length.toInt), C -> i)))

    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
  }
}


case class TupleBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends TupleWindowEncoder {

  val C: Variable = TimeVariableWithOffset("C")
  val D: Variable = TimeVariableWithOffset("D")

  val D1: Variable = TimeVariableWithOffset("D1")
  val D2: Variable = TimeVariableWithOffset("D2")

  val T1: Variable = TimeVariableWithOffset("T1")
  val T2: Variable = TimeVariableWithOffset("T2")

  val C_diff: Variable = TimeVariableWithOffset("C_diff")
  val T_plus1: Variable = TimeVariableWithOffset("T_plus1")
  val D_plus1: Variable = TimeVariableWithOffset("D_plus1")


  val spoilerAtom = Atom(Predicate(f"spoil_tu_${length}_${atom.predicate.caption}"), Atom.unapply(atom).getOrElse(Seq()))

  val baseRule: NormalRule = AspRule(windowAtomEncoding, Set(atom), Set(spoilerAtom))
  val cntSpoilerRules_1: Seq[NormalRule] = (1 to length.toInt) map { i =>
    AspRule(
      spoilerAtom,
      Set[Atom](
        atom,
        cnt(C),
        Sum(C, IntValue(-(i)), D)
      ),
      Set[Atom](
        PinnedAtom.asCount(atom, D)
      )
    )
  }

  val cntSpoilerRules_2: NormalRule = AspRule(
    spoilerAtom,
    Set[Atom](
      atom,
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

  val incrementalRule: NormalRule = AspRule(spoilerAtom, Set[Atom](atom, now(C)), Set[Atom](PinnedAtom(atom, T)))

  override def incrementalRulesAt(tick: IntValue): IncrementalRules = {
    val added = incrementalRule.assign(Assignment(Map(C -> tick, T -> tick)))
    val removed = incrementalRule.assign(Assignment(Map(C -> tick, T -> IntValue(tick.int - length.toInt))))

    // TODO: base rule is added every time - shouldn't matter because of set-semantics...
    //    IncrementalRules(added + baseRule, removed)
    null
  }
}

case class TupleAtEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, timeVariable: Time = T) extends TupleWindowEncoder {
  val D = TimeVariableWithOffset("D")
  val C = TimeVariableWithOffset("C")

  val rule: NormalRule = AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, T), Set[Atom](now(C), PinnedAtom(atom, timeVariable)))

  // at atoms got their parameter already encoded
  val allWindowRules = (0 to length.toInt) map (i => AspRule[Atom, Atom](windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom(atom, timeVariable, D), Sum(D, IntValue(-i), D))))

  override def incrementalRulesAt(i: IntValue): IncrementalRules = {
    null
  }
}