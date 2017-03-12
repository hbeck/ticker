package core

import core.asp.AspRule
import core.lars._


/**
  * Created by FM on 20.06.16.
  */
case class Ground(substitutions: Map[Variable, Value]) {

  def apply(atom: Atom): Atom = atom match {
    case a: GroundAtom => a
    case a: AtomWithArguments => this.apply(a)
  }

  def apply(atomWithArgument: AtomWithArguments): Atom = {
    val groundedArguments: Seq[Argument] = atomWithArgument.arguments map {
      // check if we have a substitution for the variable, if not use it as argument
      case v: Variable => substitutions.getOrElse(v, v)
      case v: Value => v
    }

    Atom(atomWithArgument.predicate, groundedArguments.toList)
  }

  def apply(headAtom: HeadAtom): HeadAtom = headAtom match {
    case AtAtom(time, atom) => AtAtom(time, this.apply(atom))
    case a: Atom => this.apply(a)
  }

  def apply(extendedAtom: ExtendedAtom): ExtendedAtom = extendedAtom match {
    case WindowAtom(wf, tm, a) => WindowAtom(wf, tm, this.apply(a))
    case a: Atom => this.apply(a)
  }

  def apply(rule: LarsRule): LarsRule = {
    LarsRule(
      apply(rule.head),
      rule.pos map this.apply,
      rule.neg map this.apply
    )
  }

  def apply(rule: AspRule[Atom]): AspRule[Atom] = {
    rule.from(
      apply(rule.head),
      rule.pos map this.apply,
      rule.neg map this.apply
    ).asInstanceOf[AspRule[Atom]]
  }
}


object Ground {
}