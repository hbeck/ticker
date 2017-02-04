package engine.asp

import core._
import core.asp.{AspFact, AspRule, NormalProgram, NormalRule}
import core.lars._

import scala.concurrent.duration._

//case class MappedLarsRule(larsRule: LarsRule, mappedRule:NormalRule, )


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

  def predicateFor(window: WindowAtom): Predicate = predicateFor(window.windowFunction, window.temporalModality, window.atom)

  def timePoints(unit: TimeUnit, size: Long) = Duration(unit.toMillis(size) / engineTimeUnit.toMillis, engineTimeUnit.unit).length

  def predicateFor(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) = {
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

  //  def derivation(extendedAtom: ExtendedAtom): Set[NormalRule] = extendedAtom match {
  //    case WindowAtom(k: SlidingTimeWindow, temporalModality, atom) => slidingTime(k, temporalModality, atom)
  //    case WindowAtom(n: SlidingTupleWindow, temporalModality, atom) => slidingTuple(n, temporalModality, atom)
  //    case WindowAtom(n: SlidingSpecificTupleWindow, temporalModality, atom) => slidingSpecificTuple(n, temporalModality, atom)
  //    case _ => Set()
  //  }

  def windowAtomEncoder(windowAtom: WindowAtom): WindowAtomEncoder = windowAtom match {
    case WindowAtom(window: SlidingTimeWindow, temporalModality, atom) => slidingTime(window, temporalModality, atom)
    //TODO
    //    case WindowAtom(n: SlidingTupleWindow, temporalModality, atom) => slidingTuple(n, temporalModality, atom)
    //    case WindowAtom(n: SlidingSpecificTupleWindow, temporalModality, atom) => slidingSpecificTuple(n, temporalModality, atom)
  }

  // \window^1 @_T a(X)
  // head: w_{bla}(X,T)
  def slidingTime(window: SlidingTimeWindow, temporalModality: TemporalModality, atom: Atom): WindowAtomEncoder = {
    val length = timePoints(window.windowSize.unit, window.windowSize.length)
    val head = encodedWindowAtom(WindowAtom(window, temporalModality, atom)) //TODO beautify
    temporalModality match {
      case a: At => TimeAtEncoder(length, atom, head) //TODO where is T of @_T
      case Diamond => TimeDiamondEncoder(length, atom, head)
      //case Box => TimeBoxEncoder(size, atom, head) TODO hb
    }
  }

  /*
  @deprecated
  def slidingTimeSingleRuleDepr(window: SlidingTimeWindow, temporalModality: TemporalModality, atom: Atom): Set[NormalRule] = {
    val size = window.windowSize.ticks(engineTickUnit).toInt
    val head = headFor(window, temporalModality, atom)
    val L = Variable("L")
    temporalModality match {
      case a: At => {
        val rule = AspRule[Atom, Atom](
          head(U),
          Set(now(T), atom.asAtReference(U), Leq(U, T), Sum(T, IntValue(-size), L), Leq(L, U))
        )
        //        bodiesForDiamond(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
        Set(rule)
      }
      case Diamond => {
        val rule = AspRule[Atom, Atom](
          head,
          Set(now(T), atom.asAtReference(U), Leq(U, T), Sum(T, IntValue(-size), L), Leq(L, U))
        )
        //        bodiesForDiamond(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
        Set(rule)
      }
      case Box => {
        val atoms = (0 to size) map (i => atom.asAtReference(T - i)) toSet
        val rule = AspRule[Atom, Atom](
          head,
          atoms + now(T)
        )
        Set(rule)
        //        bodiesForBox(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
      }
    }
  }

  @deprecated
  def slidingTuple(window: SlidingTupleWindow, temporalModality: TemporalModality, atom: Atom): Set[NormalRule] = {
    val size = window.windowSize
    val head = headFor(window, temporalModality, atom)
    val L = Variable("L")
    temporalModality match {
      case a: At => {
        val rule = AspRule[Atom, Atom](head(U), Set(cnt(C), atom.asCountReference(U, D), Leq(D, C), Sum(C, IntValue(1 - size.toInt), L), Leq(L, D)))
        //        bodiesForDiamond(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
        Set(rule)
      }
      case Diamond => {
        val rule = AspRule[Atom, Atom](head, Set(cnt(C), atom.asCountReference(U, D), Leq(D, C), Sum(C, IntValue(1 - size.toInt), L), Leq(L, D)))
        //        bodiesForDiamond(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
        Set(rule)
      }
      case Box => {

        val rule = AspRule[Atom, Atom](
          head,
          Set(
            cnt(C), atom.asCountReference(U, D), Sum(C, IntValue(1 - size.toInt), L),
            atom.asAtReference(L), Geq(L, U), Leq(L, T)
          )
        )

        Set(rule)
      }
    }
  }

  @deprecated //prefiltered
  def slidingSpecificTuple(window: SlidingSpecificTupleWindow, temporalModality: TemporalModality, atom: Atom): Set[NormalRule] = {
    val size = window.windowSize
    val head = headFor(window, temporalModality, atom)
    val L = Variable("L")
    temporalModality match {
      case a: At => {
        val rule = AspRule[Atom, Atom](head(U), Set(cnt(C), atom.asSpecificCountReference(U, D), Leq(D, C), Sum(C, IntValue(1 - size.toInt), L), Leq(L, D)))
        //        bodiesForDiamond(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
        Set(rule)
      }
      case Diamond => {
        val rule = AspRule[Atom, Atom](head, Set(cnt(C), atom.asSpecificCountReference(U, D), Leq(D, C), Sum(C, IntValue(1 - size.toInt), L), Leq(L, D)))
        //        bodiesForDiamond(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
        Set(rule)
      }
      case Box => {

        val rule = AspRule[Atom, Atom](
          head,
          Set(
            cnt(StringValue(atom.predicate.caption), C), atom.asSpecificCountReference(U, D), Sum(C, IntValue(1 - size.toInt), L),
            atom.asAtReference(L), Geq(L, U), Leq(L, T)
          )
        )

        Set(rule)
      }
    }
  }
  */

  //  private def headFor(window: WindowFunction, temporalModality: TemporalModality, atom: Atom): Atom = {
  //    val predicate = predicateFor(window, temporalModality, atom)
  //    AtomWithArgument(predicate, Atom.unapply(atom).getOrElse(Seq()))
  //  }

  private def encodedWindowAtom(windowAtom: WindowAtom) = {
    val predicate = predicateFor(windowAtom)
    val previousArguments = windowAtom.atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }
    //    val filteredAtomArguments = windowAtom.windowFunction match {
    //      case FluentWindow => previousArguments take 1
    //      case _ => previousArguments
    //    }
    //    val atomsWithArguments = atom(filteredAtomArguments: _*)

    windowAtom.temporalModality match {
      case At(v: Time) => Atom(predicate, previousArguments :+ v)
      case _ => Atom(predicate, previousArguments)
    }
  }

  //  def tupleReference(atom: Atom)(position: Long): GroundAtomWithArguments = atom.asTupleReference(position)

  //  def head(atom: WindowAtom): Atom = encodedWindowAtom(atom)

  //  def generateAtomsOfT(windowSize: TimeWindowSize, atom: Atom, variable: Variable, calculate: (Variable, Int) => Variable = (t, i) => t - i): Set[Atom] = {
  //    val generateAtoms = (1 to windowSize.ticks(engineTick).toInt) map (calculate(variable, _)) map (atom(_))
  //    (generateAtoms :+ atom(variable)).toSet
  //  }
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

  //val rule: NormalRule

  val allWindowRules: Seq[NormalRule] //one-shot solving: e.g. for window^3 diamond all 4 rules

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

case class TimeAtEncoder(length: Long, atom: Atom, windowAtomEncoding: Atom) extends WindowAtomEncoder {
  val N = TimeVariableWithOffset("N") //TODO hb review N is not necessarily a time variable! --> distinction between time variable and other useful?
  //TODO if we want a distinction between arbitrary variables and those with an offset, it should rather be IntVariable (which then always implicitly
  //allows the use of an offset).

  val rule: NormalRule = AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, T), Set[Atom](now(N), PinnedAtom(atom, T)))
  val allWindowRules = (0 to length.toInt) map (i => AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, T), Set[Atom](now(N), PinnedAtom(atom, T - i))))

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
  val allWindowRules = (0 to length.toInt) map (i => AspRule(windowAtomEncoding, Set[Atom](now(N), PinnedAtom(atom, T - i))))

  //TODO hb prepared for later use
  override def incrementalRulesAt(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(T -> i, N -> i)))
    val removed = rule.assign(Assignment(Map(T -> IntValue(i.int - length.toInt), N -> i)))

    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
  }
}

/*
case class TimeBox(range: Long, atom: Atom, windowAtomEncoding: Atom) extends DerivationRule {

  //TODO hb

  override val rule: NormalRule = _
  override val allRules: Seq[NormalRule] = _

  override def ruleFor(tick: IntValue): IncrementalRules = ???
}

case class TupleAt(range: Long, atom: Atom, windowAtomEncoding: Atom) extends DerivationRule {

  //TODO hb

  override val rule: NormalRule = _
  override val allRules: Seq[NormalRule] = _

  override def ruleFor(tick: IntValue): IncrementalRules = ???
}
*/

case class TupleDiamond(length: Long, atom: Atom, windowAtomEncoding: Atom) extends WindowAtomEncoder {
  val C = TimeVariableWithOffset("C")
  //TODO hb review why time variable?
  val D = Variable("D") //TODO ... and then why this not?

  val rule: NormalRule = AspRule(windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom(atom, D)))
  val allWindowRules = (0 to length.toInt) map (i => AspRule(windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom(atom, D), Sum(C, IntValue(i.toInt), D))))


  override def incrementalRulesAt(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(D -> i, C -> i)))
    val removed = rule.assign(Assignment(Map(D -> IntValue(i.int - length.toInt), C -> i)))

    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
  }
}

/*
case class TupleBox(range: Long, atom: Atom, windowAtomEncoding: Atom) extends DerivationRule {
  //TODO hb
  override val rule: NormalRule = _
  override val allRules: Seq[NormalRule] = _

  override def ruleFor(tick: IntValue): IncrementalRules = ???
}
*/
