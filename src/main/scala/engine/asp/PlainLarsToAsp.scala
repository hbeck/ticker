package engine.asp

import core._
import core.asp.{AspRule, NormalProgram, NormalRule}
import core.lars._

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

  def apply(rule: LarsRule): Set[NormalRule] = {
    val transformedRule = this.transformRule(rule)

    val derivedBodyRules = (rule.pos ++ rule.neg) flatMap derivation

    Set(transformedRule) ++ derivedBodyRules
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

    val rules = program.rules map (r => (r, this.apply(r)))

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
    Predicate(f"${windowFunction}_${operator}_${atomName.toString}")
  }

  def derivation(extendedAtom: ExtendedAtom): Set[NormalRule] = extendedAtom match {
    case WindowAtom(k: SlidingTimeWindow, temporalModality, atom) => slidingTime(k, temporalModality, atom)
    case WindowAtom(n: SlidingTupleWindow, temporalModality, atom) => slidingTuple(n, temporalModality, atom)
    case WindowAtom(n: SlidingSpecificTupleWindow, temporalModality, atom) => slidingSpecificTuple(n, temporalModality, atom)
    case _ => Set()
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

  //  def bodiesForDiamond(atom: Atom, constrainAtom: Atom, start: Long, variable: Variable): Set[Set[Atom]] = {
  //
  //    val atoms = generateAtomsOfT(start, atom, variable)
  //
  //    val bodies = atoms map (Set(constrainAtom(variable), _))
  //
  //    bodies
  //  }
  //
  //  def bodiesForBox(atom: Atom, constrainAtom: Atom, start: Long, variable: Variable): Set[Set[Atom]] = {
  //
  //    val atoms = generateAtomsOfT(start, atom, variable)
  //
  //    val body = Set(constrainAtom(variable)) ++ atoms
  //
  //    // we only have a single rule
  //    Set(body)
  //  }

  //  def bodiesForAt(atom: Atom, constrainAtom: Atom, start: Long, variable: Variable): Set[Set[Atom]] = {
  //
  //    val atoms = generateAtomsOfT(start, atom, variable)
  //
  //    val bodies = atoms map (Set(constrainAtom(variable), _))
  //
  //    bodies
  //  }


  //  def rulesForAt(at: At, atom: Atom, time: Time, constraintAtom: Atom, size: Int): Set[NormalRule] = at.time match {
  //    case t: TimePoint => rulesForAtTimePoint(headFor(), atom, constraintAtom, size, t)
  //    case v: TimeVariableWithOffset => rulesForAtTimeVariable(windowAtom, v)
  //  }

  //  def rulesForAtTimePoint(head: Atom, atom: Atom, constraintAtom: Atom, size: Long, timePoint: TimePoint): Set[NormalRule] = {
  //    val atomAtTime = atom(timePoint)
  //
  //    val rules = (0 to size.toInt) map (n => AspRule[Atom, Atom](head(n), Set(atomAtTime, constraintAtom(n))))
  //
  //    rules.toSet
  //  }
  //
  //  def rulesForAtTimeVariable(head: Atom, atom: Atom, constraintAtom: Atom, size: Long, timeVariable: TimeVariableWithOffset): Set[NormalRule] = {
  //
  //    val generatedRules = (0 to size.toInt) map { t =>
  //      AspRule(
  //        head(timeVariable - t).asInstanceOf[Atom],
  //        Set[Atom](
  //          atom(timeVariable - t),
  //          constraintAtom(T)
  //        )
  //      )
  //    }
  //    generatedRules.toSet
  //  }

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

case class TransformedLarsProgram(larsRulesAsAspRules: Seq[LarsRuleAsAspRules], staticRules: Seq[NormalRule], tickSize: EngineTick) extends NormalProgram with LarsBasedProgram {

  override val rules = (larsRulesAsAspRules flatMap { case (_, pinned) => pinned }) ++ staticRules

  override val larsRules = larsRulesAsAspRules map { case (lars, _) => lars }
}

trait IncrementalRule {

}

case class MappedLarsRule(larsRule: LarsRule, mappedRule: NormalRule, incrementalRule: IncrementalRule) {

}