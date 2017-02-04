package engine.asp

import core._
import core.asp.{AspRule, NormalProgram, NormalRule}
import core.lars._

import scala.concurrent.duration._

//case class MappedLarsRule(larsRule: LarsRule, mappedRule:NormalRule, )


/**
  * Created by fm on 20/01/2017.
  */
case class PlainLarsToAspMapper(engineTick: EngineTickUnit = 1 second) {

  def apply(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    case AtAtom(t, a) => PinnedAtom(a, t)
    case a: Atom => a
    case a: WindowAtom => this.windowAtom(a)
  }

  def apply(rule: LarsRule): MappedLarsRule = {
    val transformedRule = this.transformRule(rule)

    val derivedBodyRules = (rule.pos ++ rule.neg) flatMap derivation_

    MappedLarsRule(rule, Set(transformedRule), derivedBodyRules)
  }

  def apply(program: LarsProgram): TransformedLarsProgram = {

    val staticAtoms = program.atoms.
      flatMap(a => Seq(
        AspRule[Atom, Atom](a, Set(now(T), a.asAtReference(T))),
        AspRule[Atom, Atom](a.asAtReference(T), Set(now(T), a))
      )).
      toSeq

    val staticWindowAtom = program.windowAtoms.
      map(_.atom).
      map(a => AspRule[Atom, Atom](a.asAtReference(T), Set(a.asCountReference(T, D))))

    val static = staticAtoms ++ staticWindowAtom

    val rules = program.rules map (r => this(r))

    TransformedLarsProgram(rules, static, engineTick)
  }

  def windowAtom(windowAtom: WindowAtom): Atom = {
    val basicAtom = atomFor(windowAtom)

    basicAtom
  }

  def transformRule(rule: LarsRule): NormalRule = {
    AspRule(
      this(rule.head),
      rule.pos map this.apply,
      rule.neg map this.apply
    )
  }

  def predicateFor(window: WindowAtom): Predicate = predicateFor(window.windowFunction, window.temporalModality, window.atom)

  def predicateFor(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) = {
    val window = windowFunction match {
      case SlidingTimeWindow(size) => f"w_te_${size.ticks(engineTick)}"
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

  def derivation(extendedAtom: ExtendedAtom): Set[NormalRule] = extendedAtom match {
    case WindowAtom(k: SlidingTimeWindow, temporalModality, atom) => slidingTime(k, temporalModality, atom)
    case WindowAtom(n: SlidingTupleWindow, temporalModality, atom) => slidingTuple(n, temporalModality, atom)
    case WindowAtom(n: SlidingSpecificTupleWindow, temporalModality, atom) => slidingSpecificTuple(n, temporalModality, atom)
    case _ => Set()
  }

  def derivation_(extendedAtom: ExtendedAtom): Option[DerivationRule] = extendedAtom match {
    case WindowAtom(k: SlidingTimeWindow, temporalModality, atom) => Some(slidingTime_(k, temporalModality, atom))
    //    case WindowAtom(n: SlidingTupleWindow, temporalModality, atom) => slidingTuple(n, temporalModality, atom)
    //    case WindowAtom(n: SlidingSpecificTupleWindow, temporalModality, atom) => slidingSpecificTuple(n, temporalModality, atom)
    case _ => None
  }

  def slidingTime_(window: SlidingTimeWindow, temporalModality: TemporalModality, atom: Atom): DerivationRule = {
    val size = window.windowSize.ticks(engineTick).toInt
    val head = headFor(window, temporalModality, atom)
    temporalModality match {
      case a: At => TimeAt(size, atom, head)
      case Diamond => TimeDiamond(size, atom, head)
      case Box => TimeBox(size, atom, head)
    }
  }

  //TODO hb review this method or the one above?
  def slidingTime(window: SlidingTimeWindow, temporalModality: TemporalModality, atom: Atom): Set[NormalRule] = {
    val size = window.windowSize.ticks(engineTick).toInt
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

  private def headFor(window: WindowFunction, temporalModality: TemporalModality, atom: Atom): Atom = {
    val predicate = predicateFor(window, temporalModality, atom)
    val head = AtomWithArgument(predicate, Atom.unapply(atom).getOrElse(Seq()))
    head
  }

  private def atomFor(windowAtom: WindowAtom) = {
    val atom = PredicateAtom(predicateFor(windowAtom))
    val previousArguments = windowAtom.atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }
    val filteredAtomArguments = windowAtom.windowFunction match {
      case FluentWindow => previousArguments take 1
      case _ => previousArguments
    }
    val atomsWithArguments = atom(filteredAtomArguments: _*)

    windowAtom.temporalModality match {
      case At(v: TimeVariableWithOffset) => atomsWithArguments(v)
      case _ => atomsWithArguments
    }
  }

  //  def tupleReference(atom: Atom)(position: Long): GroundAtomWithArguments = atom.asTupleReference(position)

  //  def head(atom: WindowAtom): Atom = atomFor(atom)

  //  def generateAtomsOfT(windowSize: TimeWindowSize, atom: Atom, variable: Variable, calculate: (Variable, Int) => Variable = (t, i) => t - i): Set[Atom] = {
  //    val generateAtoms = (1 to windowSize.ticks(engineTick).toInt) map (calculate(variable, _)) map (atom(_))
  //    (generateAtoms :+ atom(variable)).toSet
  //  }
}

//TODO hb review documentation larsRulesAsAspRules vs staticRules
case class TransformedLarsProgram(larsRulesAsAspRules: Seq[MappedLarsRule], staticRules: Seq[NormalRule], tickUnit: EngineTickUnit) extends NormalProgram with LarsBasedProgram {

  override val rules = (larsRulesAsAspRules flatMap (_.mappedRules)) ++ staticRules

  override val larsRules = larsRulesAsAspRules map (_.larsRule)

  val incrementalRules = larsRulesAsAspRules flatMap (_.derivationRules)
}

case class IncrementalRules(toAdd: Seq[NormalRule], toRemove: Seq[NormalRule])

//rule to derive window atom encoding
trait DerivationRule {
  val range: Long

  val rule: NormalRule

  val allRules: Seq[NormalRule] //one-shot solving: e.g. for window^3 diamond all 4 rules

  def ruleFor(tick: IntValue): IncrementalRules
}

//TODO hb review naming
case class MappedLarsRule(larsRule: LarsRule, mappedRules: Set[NormalRule], derivationRules: Set[DerivationRule]) {
  //  val rules = mappedRules ++ incrementalRule.rule
}

//TODO hb review why is there no AtAtom?
case class TimeAt(range: Long, atom: Atom, windowAtomEncoding: Atom) extends DerivationRule {
  val N = TimeVariableWithOffset("N") //TODO hb review N is not necessarily a time variable! --> distinction between time variable and other useful?
  //TODO if we want a distinction between arbitrary variables and those with an offset, it should rather be IntVariable (which then always implicitly
  //allows the use of an offset).

  val rule: NormalRule = AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, T), Set[Atom](now(N), PinnedAtom(atom, T)))
  val allRules = (0 to range.toInt) map (i => AspRule[Atom, Atom](PinnedAtom(windowAtomEncoding, T), Set[Atom](now(N), PinnedAtom(atom, T - i))))

  override def ruleFor(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(T -> i, N -> i)))
    val removed = rule.assign(Assignment(Map(T -> IntValue(i.int - range.toInt), N -> i)))

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
case class TimeDiamond(range: Long, atom: Atom, windowAtomEncoding: Atom) extends DerivationRule {
  val N = TimeVariableWithOffset("N")

  val rule: NormalRule = AspRule(windowAtomEncoding, Set[Atom](now(N), PinnedAtom(atom, T)))
  val allRules = (0 to range.toInt) map (i => AspRule(windowAtomEncoding, Set[Atom](now(N), PinnedAtom(atom, T - i))))

  //TODO hb prepared for later use
  override def ruleFor(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(T -> i, N -> i)))
    val removed = rule.assign(Assignment(Map(T -> IntValue(i.int - range.toInt), N -> i)))

    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
  }
}

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

case class TupleDiamond(range: Long, atom: Atom, windowAtomEncoding: Atom) extends DerivationRule {
  val C = TimeVariableWithOffset("C") //TODO hb review why time variable?
  val D = Variable("D") //TODO ... and then why this not?

  val rule: NormalRule = AspRule(windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom(atom, D)))
  val allRules = (0 to range.toInt) map (i => AspRule(windowAtomEncoding, Set[Atom](cnt(C), PinnedAtom(atom, D), Sum(C, IntValue(i.toInt), D))))


  override def ruleFor(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(D -> i, C -> i)))
    val removed = rule.assign(Assignment(Map(D -> IntValue(i.int - range.toInt), C -> i)))

    IncrementalRules(Seq(AspRule(added.head, added.pos)), Seq(AspRule(removed.head, removed.pos)))
  }
}

case class TupleBox(range: Long, atom: Atom, windowAtomEncoding: Atom) extends DerivationRule {
  //TODO hb
  override val rule: NormalRule = _
  override val allRules: Seq[NormalRule] = _

  override def ruleFor(tick: IntValue): IncrementalRules = ???
}
