package engine.asp

import core._
import core.asp.{AspRule, NormalProgram, NormalRule}
import core.lars._

import scala.concurrent.duration._

/**
  * Created by fm on 20/01/2017.
  */
case class PlainLarsToAsp(engineTick: EngineTick = 1 second) {

  def apply(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    case AtAtom(t, a) => a(t)
    // TODO: discuss if this approach is correct: can an head-atom be already pinned?
    case VariablePinnedAtom(a, v: TimeVariableWithOffset) => a(v)
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
        AspRule[Atom, Atom](a, Set(now(T), a(T))),
        AspRule[Atom, Atom](a(T), Set(now(T), a))
      )).
      toSeq

    val staticWindowAtom = program.windowAtoms.
      map(_.atom).
      map(a => AspRule[Atom, Atom](a(T), Set(a.asCountReference(D))))

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
      (rule.pos map this.apply) + now(T),
      rule.neg map this.apply
    )
  }

  def predicateFor(window: WindowAtom) = predicateFor(window.windowFunction, window.temporalModality, window.atom)

  def predicateFor(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) = {
    val windowFunction = windowFunction match {
      case SlidingTimeWindow(size) => f"w_te_${size.ticks(engineTick)}"
      case SlidingTupleWindow(size) => f"w_tu_$size"
      case FluentWindow => f"w_fl"
    }
    val operator = temporalModality match {
      case Diamond => "d"
      case Box => "b"
      case a: At => f"at_${a.time}"
    }
    val atomName = atom.predicate.toString
    Predicate(f"${windowFunction}_${operator}_${atomName}")
  }

  def derivation(extendedAtom: ExtendedAtom): Set[NormalRule] = extendedAtom match {
    case w@WindowAtom(k: SlidingTimeWindow, temporalModality, atom) => slidingTime(k, temporalModality, atom)
    case w@WindowAtom(SlidingTupleWindow(n), temporalModality, atom) => slidingTuple(n, temporalModality, atom)
    //    case w@WindowAtom(SlidingTimeWindow(k), temporalModality, atom) => slidingTime(k, temporalModality, atom)
    case _ => Set()
  }

  def slidingTime(window: SlidingTimeWindow, temporalModality: TemporalModality, atom: Atom): Set[NormalRule] = {
    val size = window.windowSize.ticks(engineTick)

    temporalModality match {
      case a: At => {
        val head = headFor(window, temporalModality, atom)
        bodiesForDiamond(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
      }
      case Diamond => {
        val head = headFor(window, temporalModality, atom)
        bodiesForDiamond(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
      }
      case Box => {
        val head = headFor(window, temporalModality, atom)

        bodiesForBox(atom, now, size, T) map (body => AspRule[Atom, Atom](head, body))
      }
    }
  }

  private def headFor(window: SlidingTimeWindow, temporalModality: TemporalModality, atom: Atom) = {
    val predicate = predicateFor(window, temporalModality, atom)
    val head = AtomWithArgument(predicate, Atom.unapply(atom).getOrElse(Seq()))
    head
  }

  def slidingTuple(count: TupleCount, temporalModality: TemporalModality, atom: Atom): Set[NormalRule] = {

  }


  def bodiesForDiamond(atom: Atom, constrainAtom: Atom, start: Long, variable: Variable): Set[Set[Atom]] = {

    val atoms = generateAtomsOfT(start, atom, variable)

    val bodies = atoms map (Set(constrainAtom(variable), _))

    bodies
  }

  def bodiesForBox(atom: Atom, constrainAtom: Atom, start: Long, variable: Variable): Set[Set[Atom]] = {

    val atoms = generateAtomsOfT(start, atom, variable)

    val body = Set(constrainAtom(variable)) ++ atoms

    // we only have a single rule
    Set(body)
  }

  def bodiesForAt(atom: Atom, constrainAtom: Atom, start: Long, variable: Variable): Set[Set[Atom]] = {

    val atoms = generateAtomsOfT(start, atom, variable)

    val bodies = atoms map (Set(constrainAtom(variable), _))

    bodies
  }


    def rulesForAt(at: At, atom: Atom, time: Time, constraintAtom: Atom,size: Int): Set[NormalRule] = at.time match {
      case t: TimePoint => rulesForAtTimePoint(headFor(), atom, constraintAtom, size, t)
      case v: TimeVariableWithOffset => rulesForAtTimeVariable(windowAtom, v)
    }

  def rulesForAtTimePoint(head: Atom, atom: Atom, constraintAtom: Atom, size: Long, timePoint: TimePoint): Set[NormalRule] = {
    val atomAtTime = atom(timePoint)

    val rules = (0 to size.toInt) map (n => AspRule[Atom, Atom](head(n), Set(atomAtTime, constraintAtom(n))))

    rules.toSet
  }

  def rulesForAtTimeVariable(head: Atom, atom: Atom, constraintAtom: Atom, size: Long, timeVariable: TimeVariableWithOffset): Set[NormalRule] = {

    val generatedRules = (0 to size.toInt) map { t =>
      AspRule(
        head(timeVariable - t).asInstanceOf[Atom],
        Set[Atom](
          atom(timeVariable - t),
          constraintAtom(T)
        )
      )
    }
    generatedRules.toSet
  }

  private def atomFor(windowAtom: WindowAtom) = {
    val atom = Atom(predicateFor(windowAtom))
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

  def tupleReference(atom: Atom)(position: Long): GroundAtomWithArguments = atom.asTupleReference(position)

  def head(atom: WindowAtom): Atom = atomFor(atom)

  def generateAtomsOfT(windowSize: TimeWindowSize, atom: Atom, variable: Variable, calculate: (Variable, Int) => Variable = (t, i) => t - i): Set[Atom] = {
    val generateAtoms = (1 to windowSize.ticks(engineTick).toInt) map (calculate(variable, _)) map (atom(_))
    (generateAtoms :+ atom(variable)).toSet
  }
}

case class TransformedLarsProgram(larsRulesAsAspRules: Seq[LarsRuleAsAspRules], staticRules: Seq[NormalRule], tickSize: EngineTick) extends NormalProgram with LarsBasedProgram {

  override val rules = (larsRulesAsAspRules flatMap { case (_, pinned) => pinned }) ++ staticRules

  override val larsRules = larsRulesAsAspRules map { case (lars, _) => lars }
}
