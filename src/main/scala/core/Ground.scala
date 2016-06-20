package core

import core.asp.AspRule
import core.lars._


/**
  * Created by FM on 20.06.16.
  */
case class Ground(substitutions: Map[Variable, Value]) {
  def apply(atom: Atom): Atom = atom match {
    case a: AtomWithArgument => this.apply(a)
    case a: Atom => a
  }

  def apply(atom: AtomWithArgument): Atom = {
    val groundedArguments: Seq[Argument] = atom.arguments map {
      // check if we have a substitution for the variable, if not use it as argument
      case v: Variable => substitutions.getOrElse(v, v)
      case v: Value => v
    }

    if (groundedArguments.forall(_.isInstanceOf[Value])) {
      GroundAtom(atom.atom, groundedArguments.map(_.asInstanceOf[Value]))
    } else {
      AtomWithArguments(atom.atom, groundedArguments)
    }
  }

  def apply(headAtom: HeadAtom): HeadAtom = headAtom match {
    case AtAtom(time, atom) => AtAtom(time, this.apply(atom))
    case a: Atom => this.apply(a)
  }

  def apply(extendedAtom: ExtendedAtom): ExtendedAtom = extendedAtom match {
    case WindowAtom(wf, tm, a) => WindowAtom(wf, tm, this.apply(a))
    case a: Atom => this.apply(a)
  }

  def apply(rule: LarsRule) = {
    LarsRule(
      apply(rule.head),
      rule.pos map this.apply,
      rule.neg map this.apply
    )
  }

  def apply[TAtom <: Atom](rule: AspRule[TAtom]) = {
    AspRule(
      apply(rule.head),
      rule.pos map this.apply,
      rule.neg map this.apply
    )
  }
}


object Ground {
}