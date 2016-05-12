package engine

import core.asp.{AspFact, AspRule}
import core.lars._
import core.{Atom, AtomWithArguments}

/**
  * Created by FM on 05.05.16.
  */
object PlainLarsToAsp {
  // TODO: discuss if now should also be moved into core.lars._
  val now = Atom("now")


  def apply(headAtom: HeadAtom): Atom = headAtom match {
    case AtAtom(t, a) => a(t.toString)
    case a: Atom => a(T)
  }

  def apply(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    case AtAtom(t, a) => a(t.toString)
    case a: Atom => a(T)
    case a: WindowAtom => this.apply(a)
  }

  //main
  def apply(rule: Rule): Set[AspRule] = {
    val rulesForBody = (rule.pos ++ rule.neg) flatMap additionalRules

    Set(this.rule(rule)) ++ rulesForBody
  }

  def apply(program: Program): Seq[AspRule] = {
    program.rules flatMap this.apply
  }

  def apply(windowAtom: WindowAtom) = {
    val arguments = windowAtom.atom match {
      case AtomWithArguments(_, arguments) => arguments :+ T
      case a: Atom => Seq(T)
    }
    Atom(nameFor(windowAtom))(arguments: _*)
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
    val generatedAtoms = generateAtomsOfT(windowAtom.windowFunction, windowAtom.atom, generateTimeVariable)

    val posBody = generatedAtoms ++ Set(now(T), windowAtom.atom(T))

    Set(AspRule(Atom(nameFor(windowAtom)), posBody.toSet))
  }

  def rulesForDiamond(windowAtom: WindowAtom): Set[AspRule] = {
    val head = Atom(nameFor(windowAtom))

    val generatedAtoms = generateAtomsOfT(windowAtom.windowFunction, windowAtom.atom, generateTimeVariable)

    val rules = generatedAtoms map (a => AspRule(head, Set(now(T), a)))

    rules.toSet
  }

  def rulesForAt(windowAtom: WindowAtom): Set[AspRule] = {
    val at = windowAtom.temporalModality.asInstanceOf[At]
    val head = Atom(nameFor(windowAtom))

    val atomAtTime = windowAtom.atom(at.time.toString)

    val nowAtoms = generateAtomsOfT(windowAtom.windowFunction, now, x => (at.time.timePoint - x).toString)

    val rules = nowAtoms map (n => AspRule(head, Set(atomAtTime, n)))

    rules.toSet
  }

  def generateTimeVariable(increment: Int): String = {
    if (increment == 0)
      return T
    T + "-" + increment
  }

  def generateAtomsOfT(windowFunction: WindowFunction, atom: Atom, increment: Int => String): Set[Atom] = {
    val windowSize: Long = windowFunction match {
      case SlidingTimeWindow(size) => size
    }

    // TODO: current implementation of (... to ...) only works with ints
    // luck us if we run out of int's on enumerating
    val generateAtoms = (1 to windowSize.toInt) map increment map (atom(_))
    (generateAtoms :+ atom(increment(0))).toSet
  }
}
