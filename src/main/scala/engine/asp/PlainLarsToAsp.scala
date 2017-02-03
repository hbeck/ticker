package engine.asp

import core._
import core.asp.{AspRule, NormalProgram, NormalRule}
import core.lars._
import engine.asp.tms.Pin

import scala.concurrent.duration._

//case class MappedLarsRule(larsRule: LarsRule, mappedRule:NormalRule, )


/**
  * Created by fm on 20/01/2017.
  */
case class PlainLarsToAsp(engineTick: EngineTick = 1 second) {

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

    val rules = program.rules map (r => this.apply(r))

    TransformedLarsProgram(rules, static, engineTick)
  }

  def windowAtom(windowAtom: WindowAtom): Atom = {
    val basicAtom = atomFor(windowAtom)

    basicAtom
  }

  def transformRule(rule: LarsRule): NormalRule = {
    AspRule(
      this.apply(rule.head),
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

  def derivation_(extendedAtom: ExtendedAtom): Option[IncrementalRule] = extendedAtom match {
    case WindowAtom(k: SlidingTimeWindow, temporalModality, atom) => Some(slidingTime_(k, temporalModality, atom))
    //    case WindowAtom(n: SlidingTupleWindow, temporalModality, atom) => slidingTuple(n, temporalModality, atom)
    //    case WindowAtom(n: SlidingSpecificTupleWindow, temporalModality, atom) => slidingSpecificTuple(n, temporalModality, atom)
    case _ => None
  }

  def slidingTime_(window: SlidingTimeWindow, temporalModality: TemporalModality, atom: Atom): IncrementalRule = {
    val size = window.windowSize.ticks(engineTick).toInt
    val head = headFor(window, temporalModality, atom)
    temporalModality match {
      case a: At => {
        val incremental = TimeAt(size, atom, head)
        incremental
      }
      case Diamond => TimeDiamond(size, atom, head)
    }
  }

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
    val atomsWithArguments = atom.apply(filteredAtomArguments: _*)

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

case class TransformedLarsProgram(larsRulesAsAspRules: Seq[MappedLarsRule], staticRules: Seq[NormalRule], tickSize: EngineTick) extends NormalProgram with LarsBasedProgram {

  override val rules = (larsRulesAsAspRules flatMap {
    _.mappedRules
  }) ++ staticRules

  override val larsRules = larsRulesAsAspRules map (_.larsRule)
}

case class IncrementalRules(toAdd: Seq[NormalRule], toRemove: Seq[NormalRule])

trait IncrementalRule {
  val range: Long

  val rule: NormalRule

  def ruleFor(tick: IntValue): IncrementalRules
}

case class MappedLarsRule(larsRule: LarsRule, mappedRules: Set[NormalRule], incrementalRule: Set[IncrementalRule]) {
  //  val rules = mappedRules ++ incrementalRule.rule
}

case class TimeDiamond(range: Long, atom: Atom, windowAtom: Atom) extends IncrementalRule {
  val N = TimeVariableWithOffset("N")

  val rule: NormalRule = AspRule.apply(windowAtom, Set[Atom](now(N), PinnedAtom(atom, T)))

  override def ruleFor(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(T -> i, N -> i)))
    val removed = rule.assign(Assignment(Map(T -> IntValue(i.int - range.toInt), N -> i)))

    IncrementalRules(Seq(AspRule.apply(added.head, added.pos)), Seq(AspRule.apply(removed.head, removed.pos)))
  }
}

case class TimeAt(range: Long, atom: Atom, windowAtom: Atom) extends IncrementalRule {
  val N = TimeVariableWithOffset("N")

  val rule: NormalRule = AspRule[Atom, Atom](PinnedAtom(windowAtom, T), Set[Atom](now(T), PinnedAtom(atom, T)))

  override def ruleFor(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(T -> i)))
    val removed = rule.assign(Assignment(Map(T -> IntValue(i.int - range.toInt))))

    IncrementalRules(Seq(AspRule.apply(added.head, added.pos)), Seq(AspRule.apply(removed.head, removed.pos)))
  }
}

case class TupleDiamond(range: Long, atom: Atom, windowAtom: Atom) extends IncrementalRule {
  val C = TimeVariableWithOffset("C")
  val D = Variable("D")

  val rule: NormalRule = AspRule.apply(windowAtom, Set[Atom](cnt(C), PinnedAtom(atom, D)))

  override def ruleFor(i: IntValue): IncrementalRules = {
    val added = rule.assign(Assignment(Map(D -> i, C -> i)))
    val removed = rule.assign(Assignment(Map(D -> IntValue(i.int - range.toInt), C -> i)))

    IncrementalRules(Seq(AspRule.apply(added.head, added.pos)), Seq(AspRule.apply(removed.head, removed.pos)))
  }
}
