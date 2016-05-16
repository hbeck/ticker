package engine.asp

import core.asp.{AspProgram, AspRule}
import core.lars._
import core.{Atom, AtomWithArguments}
import engine._

/**
  * Created by FM on 05.05.16.
  */
object PlainLarsToAsp {

  def apply(headAtom: HeadAtom): Atom = headAtom match {
    case AtAtom(t, a) => a(t)
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
    val arguments = windowAtom.atom match {
      case AtomWithArguments(_, arg) => arg :+ T.toString
      case a: Atom => Seq(T.toString)
    }
    val basicAtom = Atom(nameFor(windowAtom))

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
      case AtomWithArguments(atom, _) => atom.toString
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
    val h = head(windowAtom)

    val atomAtTime = windowAtom.atom(at.time)
    val referenceTime = at.time match {
      case t: TimePoint => t
      case _ => T
    }

    val nowAtoms = generateAtomsOfT(windowAtom.windowFunction, now, referenceTime)

    val rules = nowAtoms map (n => AspRule(h, Set(atomAtTime, n)))

    rules.toSet
  }

  def head(atom: WindowAtom) = Atom(nameFor(atom))(T)

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
