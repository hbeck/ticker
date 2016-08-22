package engine.asp

import core.asp.AspRule
import core.lars._
import core._

/**
  * Created by FM on 05.05.16.
  */
object PlainLarsToAsp {

  def apply(headAtom: HeadAtom): AtomWithArgument = headAtom match {
    case AtAtom(t, a) => a(t)
    // TODO: discuss if this approach is correct: can an head-atom be already pinned?
    case PinnedAtom(a, v: TimeVariableWithOffset) => a(v)
    case a: Atom => a(T)
  }

  def apply(extendedAtom: ExtendedAtom): AtomWithArgument = extendedAtom match {
    case AtAtom(t, a) => a(t)
    case a: Atom => a(T)
    case a: WindowAtom => this.apply(a)
  }

  def apply(rule: LarsRule): Set[PinnedRule] = {
    val rulesForBody = (rule.pos ++ rule.neg) flatMap additionalRules

    Set(this.rule(rule)) ++ rulesForBody
  }

  def apply(program: LarsProgram): MappedProgram = {
    val rules: Seq[LarsRuleMapping] = program.rules map (r => (r, this.apply(r)))
    MappedProgram(rules)
  }

  def apply(windowAtom: WindowAtom): PinnedAtom = {
    val basicAtom = atomFor(windowAtom)

    basicAtom(T)
  }

  def rule(rule: LarsRule): PinnedRule = {
    AspRule(
      this.apply(rule.head),
      (rule.pos map this.apply) + now(T),
      rule.neg map this.apply
    )
  }

  def nameFor(window: WindowAtom) = {
    val windowFunction = window.windowFunction match {
      case SlidingTimeWindow(size) => f"w_te_$size"
      case SlidingTupleWindow(size) => f"w_tu_$size"
      case FluentWindow => f"w_fl"
    }
    val operator = window.temporalModality match {
      case Diamond => "d"
      case Box => "b"
      case a: At => f"at_${a.time}"
    }
    val atomName = window.atom.predicate.toString
    f"${windowFunction}_${operator}_${atomName}"
  }

  def additionalRules(extendedAtom: ExtendedAtom): Set[PinnedRule] = extendedAtom match {
    case w@WindowAtom(_, temporalOperator, _) => temporalOperator match {
      case a: At => rulesForAt(w)
      case Diamond => rulesForDiamond(w)
      case Box => rulesForBox(w)
    }
    case _ => Set()
  }

  def rulesForBox(windowAtom: WindowAtom): Set[PinnedRule] = {
    val generatedAtoms: Set[AtomWithArgument] = windowAtom.windowFunction match {
      case SlidingTimeWindow(size) => generateAtomsOfT(size, windowAtom.atom, T)
      case SlidingTupleWindow(size) => {
        val rAtom = tupleReference(windowAtom.atom) _
        (0 to (size.toInt-1)) map (rAtom(_)) toSet
      }
    }

    val posBody = generatedAtoms ++ Set(now(T), windowAtom.atom(T))

    Set(AspRule(head(windowAtom), posBody, Set()))
  }

  def rulesForDiamond(windowAtom: WindowAtom): Set[PinnedRule] = {
    val h = head(windowAtom)

    val generatedAtoms = windowAtom.windowFunction match {
      case SlidingTimeWindow(size) => generateAtomsOfT(size, windowAtom.atom, T)
      case SlidingTupleWindow(size) => {
        val rAtom = tupleReference(windowAtom.atom) _
        (0 to (size.toInt - 1)) map (rAtom(_)) toSet
      }
    }

    val rules = generatedAtoms map (a => AspRule(h, Set(now(T), a)))

    rules.toSet
  }

  def rulesForAt(windowAtom: WindowAtom): Set[PinnedRule] = {
    val at = windowAtom.temporalModality.asInstanceOf[At]

    at.time match {
      case t: TimePoint => rulesForAtTimePoint(windowAtom, t)
      case v: TimeVariableWithOffset => rulesForAtTimeVariable(windowAtom, v)
    }
  }

  def rulesForAtTimePoint(windowAtom: WindowAtom, timePoint: TimePoint): Set[PinnedRule] = {
    val h = head(windowAtom)

    val atomAtTime = windowAtom.atom(timePoint)

    val nowAtoms = windowAtom.windowFunction match {
      case SlidingTimeWindow(size) => generateAtomsOfT(size, now, timePoint)
    }

    val rules = nowAtoms map (n => AspRule(h, Set(atomAtTime, n)))

    rules.toSet
  }

  def rulesForAtTimeVariable(windowAtom: WindowAtom, timeVariable: TimeVariableWithOffset): Set[PinnedRule] = {
    val reachAtom = Atom("reach_" + nameFor(windowAtom))

    // we need the reach atom in the form of atom(T-k,T)
    val reachAtoms = windowAtom.windowFunction match {
      case SlidingTimeWindow(size) => generateAtomsOfT(size, reachAtom, T) map (a => a(T))
    }

    val reachRules = reachAtoms.toSet[AtomWithArgument] map (r => AspRule(r, Set[AtomWithArgument](now(T))))

    val windowRule = AspRule(head(windowAtom), Set[AtomWithArgument](now(T), windowAtom.atom(timeVariable), reachAtom(timeVariable)(T)))

    (reachRules + windowRule).toSet
  }

  private def atomFor(windowAtom: WindowAtom) = {
    val atom = Atom(nameFor(windowAtom))
    val previousArguments = windowAtom.atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }
    val atomsWithArguments = atom.apply(previousArguments: _*)

    windowAtom.temporalModality match {
      case At(v: TimeVariableWithOffset) => atomsWithArguments(v)
      case _ => atomsWithArguments
    }
  }

  def tupleReference(atom: Atom)(position: Long): GroundAtomWithArguments = atom.asTupleReference(position)

  def head(atom: WindowAtom): AtomWithArgument = atomFor(atom)(T)

  def generateAtomsOfT(windowSize: Long, atom: Atom, referenceTime: Time): Set[AtomWithArgument] = {
    // TODO: current implementation of (... to ...) only works with integer
    val generateAtoms = (1 to windowSize.toInt) map (referenceTime - _) map (atom(_))
    (generateAtoms :+ atom(referenceTime)).toSet
  }
}

/*
 * we keep the original lars rule for potential later optimizations
 */
case class MappedProgram(mappedRules: Seq[LarsRuleMapping]) extends PinnedProgram {
  override val rules = mappedRules.flatMap(_._2)

  val windowAtoms = mappedRules.
    map(_._1).
    flatMap(r => r.body collect { case w: WindowAtom => w })

  val slidingWindows = windowAtoms collect { case s: SlidingWindow => s }

  // TODO: when there are no windows - is the maximumWindowSize 0 or None?
  val maximumWindowSize: WindowSize = slidingWindows.isEmpty match {
    case false => slidingWindows.maxBy(_.windowSize).windowSize
    case true => 0
  }
}