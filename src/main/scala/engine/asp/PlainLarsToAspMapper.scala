package engine.asp

import core._
import core.asp.{AspFact, AspRule, NormalProgram, NormalRule}
import core.lars._

import scala.concurrent.duration._

/**
  * Created by fm on 20/01/2017.
  */
case class PlainLarsToAspMapper(engineTimeUnit: EngineTimeUnit = 1 second) {

  def encodingAtom(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    case AtAtom(t, a) => PinnedAtom(a, t)
    case a: Atom => a
    case a: WindowAtom => this.encodedWindowAtom(a)
  }

  def encodeRule(rule: LarsRule): LarsRuleEncoding = {
    val encodedRule = this.encode(rule)

    val windowAtomEncoders = (rule.pos ++ rule.neg) collect {
      case wa: WindowAtom => windowAtomEncoder(wa)
    }

    LarsRuleEncoding(rule, Set(encodedRule), windowAtomEncoders)
  }

  def apply(program: LarsProgram): LarsProgramEncoding = {

    val nowAndAtNowIdentityRules = program.atoms.
      flatMap { a =>
        Seq(
          AspRule[Atom, Atom](a, Set(now.apply(T), PinnedAtom(a, T))),
          AspRule[Atom, Atom](PinnedAtom(a, T), Set(now(T), a))
        )
      }.toSeq

    val rulesEncodings = program.rules map encodeRule

    val backgroundData = Set[Atom]() //TODO

    LarsProgramEncoding(rulesEncodings, nowAndAtNowIdentityRules, backgroundData, engineTimeUnit)
  }

  def encode(rule: LarsRule): NormalRule = {
    AspRule(
      encodingAtom(rule.head),
      rule.pos map this.encodingAtom,
      rule.neg map this.encodingAtom
    )
  }

  def windowAtomEncoder(windowAtom: WindowAtom): WindowAtomEncoder = windowAtom match {
    case WindowAtom(window: SlidingTimeWindow, temporalModality, atom) => slidingTime(window, temporalModality, atom)
    case WindowAtom(window: SlidingTupleWindow, temporalModality, atom) => slidingTuple(window, temporalModality, atom)
  }

  // \window^1 @_T a(X)
  // head: w_{bla}(X,T)
  def slidingTime(window: SlidingTimeWindow, temporalModality: TemporalModality, atom: Atom): WindowAtomEncoder = {
    val length = timePoints(window.windowSize.unit, window.windowSize.length)
    val head = encodedWindowAtom(WindowAtom(window, temporalModality, atom)) //TODO beautify
    temporalModality match {
      case a: At => TimeAtEncoder(length, atom, head, a.time)
      case Diamond => TimeDiamondEncoder(length, atom, head)
      case Box => TimeBoxEncoder(length, atom, head)
    }
  }

  def slidingTuple(window: SlidingTupleWindow, temporalModality: TemporalModality, atom: Atom): WindowAtomEncoder = {
    val head = encodedWindowAtom(WindowAtom(window, temporalModality, atom)) //TODO beautify
    temporalModality match {
      case Diamond => TupleDiamondEncoder(window.windowSize, atom, head)
      case Box => TupleBoxEncoder(window.windowSize, atom, head)
      case a: At => TupleAtEncoder(window.windowSize, atom, head, a.time)
    }
  }

  private def encodedWindowAtom(windowAtom: WindowAtom) = {
    val predicate = predicateFor(windowAtom)
    val previousArguments = windowAtom.atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }

    windowAtom.temporalModality match {
      case At(v: Time) => Atom(predicate, previousArguments :+ v)
      case _ => Atom(predicate, previousArguments)
    }
  }

  private def timePoints(unit: TimeUnit, size: Long) = Duration(unit.toMillis(size) / engineTimeUnit.toMillis, engineTimeUnit.unit).length

  private def predicateFor(window: WindowAtom): Predicate = predicateFor(window.windowFunction, window.temporalModality, window.atom)

  private def predicateFor(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) = {
    val window = windowFunction match {
      case SlidingTimeWindow(size) => f"w_te_${timePoints(size.unit, size.length)}"
      case SlidingTupleWindow(size) => f"w_tu_$size"
      case FluentWindow => f"w_fl"
    }
    val operator = temporalModality match {
      case Diamond => "d"
      case Box => "b"
      case a: At => f"at_${a.time}"
    }
    val atomName = atom match {
      case p: PinnedAtom => p.atom.predicate
      case _ => atom.predicate
    }
    Predicate(f"${window}_${operator}_${atomName.toString}")
  }
}

case class LarsProgramEncoding(larsRuleEncodings: Seq[LarsRuleEncoding], nowAndAtNowIdentityRules: Seq[NormalRule], backgroundData: Set[Atom], tickUnit: EngineTimeUnit) extends NormalProgram with LarsBasedProgram {

  override val rules = (larsRuleEncodings flatMap (_.ruleEncodings)) ++ nowAndAtNowIdentityRules ++ (backgroundData map (AspFact(_))) //for one-shot solving

  override val larsRules = larsRuleEncodings map (_.larsRule)

  val windowAtomEncoders = larsRuleEncodings flatMap (_.windowAtomEncoders)
}

case class IncrementalRules(toAdd: Seq[NormalRule], toRemove: Seq[NormalRule])

//to derive window atom encoding
trait WindowAtomEncoder {
  val length: Long

  val allWindowRules: Seq[NormalRule] //one-shot/reactive clingo solving: e.g. for window^3 diamond all 4 rules

  def incrementalRulesAt(tick: IntValue): IncrementalRules
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
case class LarsRuleEncoding(larsRule: LarsRule, ruleEncodings: Set[NormalRule], windowAtomEncoders: Set[WindowAtomEncoder]) {
}

case class TimeAtEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, time: Time = T) extends WindowAtomEncoder {
  val N = TimeVariableWithOffset("N") //TODO hb review N is not necessarily a time variable! --> distinction between time variable and other useful?
  //TODO if we want a distinction between arbitrary variables and those with an offset, it should rather be IntVariable (which then always implicitly
  //allows the use of an offset).

  val rule: NormalRule = AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, time), Set[Atom](now(N), PinnedAtom(atom, time)))
  val allWindowRules = (0 to length.toInt) map (i => AspRule[Atom, Atom](windowAtomEncoding, Set[Atom](now(N), PinnedAtom(atom, time - i))))

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
case class TimeDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends WindowAtomEncoder {
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


case class TimeBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends WindowAtomEncoder {

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

case class TupleDiamondEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends WindowAtomEncoder {
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


case class TupleBoxEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends WindowAtomEncoder {

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


case class TupleAtEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom, timeVariable: Time = T) extends WindowAtomEncoder {
  val D = TimeVariableWithOffset("D")
  val C = TimeVariableWithOffset("C")

  val rule: NormalRule = AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, T), Set[Atom](now(C), PinnedAtom(atom, timeVariable)))

  // at atoms got their parameter already encoded
  val allWindowRules = (0 to length.toInt) map (i => AspRule[Atom, Atom](windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom(atom, timeVariable, D), Sum(D, IntValue(-i), D))))

  override def incrementalRulesAt(i: IntValue): IncrementalRules = {
    null
  }
}
