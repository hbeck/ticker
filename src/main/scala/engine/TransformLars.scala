package engine

import core.asp.{AspFact, AspRule}
import core.{AtomWithArguments, Atom}
import core.lars._
import engine.implementations.StreamingAspTransformation

import scala.collection.immutable

/**
  * Created by FM on 05.05.16.
  */
object TransformLars {
  val now = StreamingAspTransformation.now
  val T = "T"

  def apply(headAtom: HeadAtom): Atom = headAtom match {
    case a: AtAtom => this.apply(a)
    case a: Atom => this.apply(a)
  }

  def apply(extendedAtom: ExtendedAtom): Atom = extendedAtom match {
    case a: AtAtom => this.apply(a)
    case a: Atom => this.apply(a)
    case a: WindowAtom => this.apply(a)
  }

  def apply(rule: Rule, time: Time): Set[AspRule] = {
    val rulesForBody = (rule.pos ++ rule.neg) flatMap (this.ruleFor(_))

    // TODO: now(time.toString) makes only sense for a program (once, not for every rule)
    Set(this.rule(rule)) ++ rulesForBody ++ Set(AspFact(now(time.toString)))
  }

  def apply(program: Program, time: Time): Set[AspRule] = {
    program.rules flatMap (this.apply(_, time))
  }

  def apply(atom: Atom): Atom = {
    atom(T)
  }

  def apply(atAtom: AtAtom): Atom = {
    atAtom.atom(atAtom.time.toString)
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
      rule.pos map this.apply,
      rule.neg map this.apply
    )
  }

  def nameFor(window: WindowAtom) = {
    val windowFunction = window.windowFunction match {
      case SlidingTimeWindow(size) => f"w_$size"
    }
    val operator = window.temporalOperator match {
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

  def ruleFor(extendedAtom: ExtendedAtom): Set[AspRule] = extendedAtom match {
    case w: WindowAtom => ruleForWindow(w)
    case _ => Set()
  }

  def ruleForWindow(windowAtom: WindowAtom): Set[AspRule] = windowAtom.temporalOperator match {
    case Box => Set(ruleForBox(windowAtom))
    case Diamond => ruleForDiamond(windowAtom)
    case a: At => ruleForAt(windowAtom)
  }

  def ruleForBox(windowAtom: WindowAtom): AspRule = {
    val generatedAtoms = generateAtomsOfT(windowAtom.windowFunction, windowAtom.atom, generateTimeVariable)

    val posBody = generatedAtoms ++ Set(TransformLars(now), TransformLars(windowAtom.atom))

    AspRule(Atom(nameFor(windowAtom)), posBody.toSet)
  }

  def ruleForDiamond(windowAtom: WindowAtom): Set[AspRule] = {
    val head = Atom(nameFor(windowAtom))

    val generatedAtoms = generateAtomsOfT(windowAtom.windowFunction, windowAtom.atom, generateTimeVariable)

    val rules = generatedAtoms map (a => AspRule(head, Set(now(T), a)))

    rules.toSet
  }

  def ruleForAt(windowAtom: WindowAtom): Set[AspRule] = {
    val at = windowAtom.temporalOperator.asInstanceOf[At]
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
    val windowSize = windowFunction match {
      case SlidingTimeWindow(size) => size
    }

    val generateAtoms = (1 to windowSize) map increment map (atom(_))
    (generateAtoms :+ atom(increment(0))).toSet
  }
}
