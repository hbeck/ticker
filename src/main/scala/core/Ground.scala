package core

import core.lars._


/**
  * Created by FM on 20.06.16.
  */
case class Ground(substitutions: Map[Variable, Value]) {

  def apply(atom: Atom): Atom = atom match {
    case a: GroundAtom => a
    case a: AtomWithArgument => this.apply(a)
  }

  def apply(atomWithArgument: AtomWithArgument): Atom = {
    val groundedArguments: Seq[Argument] = atomWithArgument.arguments map {
      // check if we have a substitution for the variable, if not use it as argument
      case v: Variable => substitutions.getOrElse(v, v)
      case v: Value => v
    }

    AtomModification(atomWithArgument)(groundedArguments.toList)
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

  //  def apply[TAtom <: Atom](rule: AspRule[TAtom]):AspRule[TAtom] = {
  //    AspRule(
  //      apply(rule.head),
  //      rule.pos map this.apply,
  //      rule.neg map this.apply
  //    )
  //  }
}


object Ground {
}