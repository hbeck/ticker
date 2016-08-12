package engine.asp

import core.asp.AspRule
import core.lars._
import core.{Atom, AtomWithArgument, PinnedAtom}

/**
  * Created by FM on 05.05.16.
  */
object PlainLarsToAsp {

  def apply(headAtom: HeadAtom): PinnedAtom = headAtom match {
    case AtAtom(t, a) => a(t)
    // TODO: discuss if this approach is correct: can an head-atom be already pinned?
    case PinnedAtom(a, v: TimeVariableWithOffset) => a(v)
    case a: Atom => a(T)
  }

  def apply(extendedAtom: ExtendedAtom): PinnedAtom = extendedAtom match {
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
    }
    val operator = window.temporalModality match {
      case Diamond => "d"
      case Box => "b"
      case a: At => f"at_${a.time}"
    }
    val atomName = window.atom match {
      case aa: AtomWithArgument => aa.atom.toString
      case a: Atom => a.toString
    }
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
    val generatedAtoms = generateAtomsOfT(windowAtom.windowFunction, windowAtom.atom, T)

    val posBody = generatedAtoms ++ Set(now(T), windowAtom.atom(T))

    Set(AspRule(head(windowAtom), posBody, Set()))
  }

  def rulesForDiamond(windowAtom: WindowAtom): Set[PinnedRule] = {
    val h = head(windowAtom)

    val generatedAtoms = generateAtomsOfT(windowAtom.windowFunction, windowAtom.atom, T)

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

    val nowAtoms = generateAtomsOfT(windowAtom.windowFunction, now, timePoint)

    val rules = nowAtoms map (n => AspRule(h, Set(atomAtTime, n)))

    rules.toSet
  }

  def rulesForAtTimeVariable(windowAtom: WindowAtom, timeVariable: TimeVariableWithOffset): Set[PinnedRule] = {
    val reachAtom = Atom("reach_" + nameFor(windowAtom))

    // we need the reach atom in the form of atom(T-k,T)
    val reachAtoms = generateAtomsOfT(windowAtom.windowFunction, reachAtom, T) map (a => a(T))

    val reachRules = reachAtoms map (r => AspRule(r, Set(now(T))))

    val windowRule = AspRule(head(windowAtom), Set(now(T), windowAtom.atom(timeVariable), reachAtom(timeVariable)(T)))

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

  def head(atom: WindowAtom) = atomFor(atom)(T)

  def generateAtomsOfT(windowFunction: WindowFunction, atom: Atom, referenceTime: Time): Set[PinnedAtom] = {
    val windowSize: Long = windowFunction match {
      case SlidingTimeWindow(size) => size
    }

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
}