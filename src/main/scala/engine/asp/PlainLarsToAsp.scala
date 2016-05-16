package engine.asp

import core.asp.{AspFact, AspProgram, AspRule}
import core.lars._
import core.{Atom, AtomWithArgument, AtomWithArguments, AtomWithTime}
import engine._

/**
  * Created by FM on 05.05.16.
  */
object PlainLarsToAsp {

  def apply(headAtom: HeadAtom): Atom = headAtom match {
    case AtAtom(t, a) => a(t)
      // TODO: discuss if this approach is correct
    case AtomWithTime(a, v: TimeVariable) => a(v)
    case a: Atom => a(T)
  }

  def apply(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    case AtAtom(t, a) => a(t)
    case a: Atom => a(T)
    case a: WindowAtom => this.apply(a)
  }

  def apply(rule: Rule): Set[AspRule] = {
    val rulesForBody = (rule.pos ++ rule.neg) flatMap additionalRules

    Set(this.rule(rule)) ++ rulesForBody
  }

  def apply(program: Program): AspProgram = {
    val rules = program.rules flatMap this.apply
    AspProgram(rules.toList)
  }

  def apply(windowAtom: WindowAtom) = {

    // TODO should not be needed
    val arguments = windowAtom.atom match {
      case aa: AtomWithArgument => aa.arguments :+ T.toString
      case a: Atom => Seq(T.toString)
    }
    val basicAtom = atomFor(windowAtom)

    basicAtom(arguments: _*)
  }

  def rule(rule: Rule): AspRule = {
    AspRule(
      this.apply(rule.head),
      (rule.pos map this.apply) + now(T),
      rule.neg map this.apply
    )
  }

  def nameFor(window: WindowAtom) = {
    val windowFunction = window.windowFunction match {
      case SlidingTimeWindow(size) => f"w_$size"
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

  def additionalRules(extendedAtom: ExtendedAtom): Set[AspRule] = extendedAtom match {
    case w@WindowAtom(_, temporalOperator, _) => temporalOperator match {
      case a: At => rulesForAt(w)
      case Diamond => rulesForDiamond(w)
      case Box => rulesForBox(w)
    }
    case _ => Set()
  }

  def rulesForBox(windowAtom: WindowAtom): Set[AspRule] = {
    val generatedAtoms = generateAtomsOfT(windowAtom.windowFunction, windowAtom.atom, T)

    val posBody = generatedAtoms ++ Set(now(T), windowAtom.atom(T))

    Set(AspRule(head(windowAtom), posBody))
  }

  def rulesForDiamond(windowAtom: WindowAtom): Set[AspRule] = {
    val h = head(windowAtom)

    val generatedAtoms = generateAtomsOfT(windowAtom.windowFunction, windowAtom.atom, T)

    val rules = generatedAtoms map (a => AspRule(h, Set(now(T), a)))

    rules.toSet
  }

  def rulesForAt(windowAtom: WindowAtom): Set[AspRule] = {
    val at = windowAtom.temporalModality.asInstanceOf[At]

    at.time match {
      case t: TimePoint => rulesForAtTimePoint(windowAtom, t)
      case v: TimeVariable => rulesForAtTimeVariable(windowAtom, v)
    }
  }

  def rulesForAtTimePoint(windowAtom: WindowAtom, timePoint: TimePoint): Set[AspRule] = {
    val h = head(windowAtom)

    val atomAtTime = windowAtom.atom(timePoint)

    val nowAtoms = generateAtomsOfT(windowAtom.windowFunction, now, timePoint)

    val rules = nowAtoms map (n => AspRule(h, Set(atomAtTime, n)))

    rules.toSet
  }

  def rulesForAtTimeVariable(windowAtom: WindowAtom, timeVariable: TimeVariable): Set[AspRule] = {
    val reachAtom = Atom("reach_" + nameFor(windowAtom))

    // we need the reach atom in the form of atom(T-k,T)
    val reachAtoms = generateAtomsOfT(windowAtom.windowFunction, reachAtom, T) map (a => a(T))

    val reachRules = reachAtoms map (r => AspRule(r, now(T)))

    val windowRule = AspRule(head(windowAtom), Set(now(T), windowAtom.atom(timeVariable), reachAtom(timeVariable)(T)))

    (reachRules + windowRule).toSet
  }

  private def atomFor(windowAtom: WindowAtom) = {
    val atom = Atom(nameFor(windowAtom))
    windowAtom.temporalModality match {
      case At(v: TimeVariable) => atom(v)
      case _ => atom
    }
  }

  def head(atom: WindowAtom) = atomFor(atom)(T)

  def generateAtomsOfT(windowFunction: WindowFunction, atom: Atom, referenceTime: Time): Set[Atom] = {
    val windowSize: Long = windowFunction match {
      case SlidingTimeWindow(size) => size
    }

    // TODO: current implementation of (... to ...) only works with ints
    // luck us if we run out of int's on enumerating
    val generateAtoms = (1 to windowSize.toInt) map (referenceTime - _) map (atom(_))
    (generateAtoms :+ atom(referenceTime)).toSet
  }
}
