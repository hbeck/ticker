package core.asp

import core._
import core.lars.{ExtendedAtom, HeadAtom}

import scala.language.implicitConversions

/**
  * Created by FM on 27.02.16.
  */
class RuleBuilder(bodyPos: Set[Atom] = Set(), bodyNeg: Set[Atom] = Set()) {
  def pos(atoms: Atom*) = new RuleBuilder(bodyPos ++ atoms, bodyNeg)

  def neg(atoms: Atom*) = new RuleBuilder(bodyPos, bodyNeg ++ atoms)

  def head(head: Atom) = new UserDefinedAspRule(head, bodyPos, bodyNeg)
}


class BuilderHead(val head: Atom) {
//  def :- = new NotBuilderCollection(head, Set[Atom](), Set[Atom]())

  def :-(atom: Atom) = new NotBuilderCollection(head, Set(atom), Set[Atom]())

  def :-(notAtom: not) = new NotBuilderCollection(head, Set(), Set(notAtom.atom))

  //  def :-(item: BuilderItem[Atom]) = item match {
  //    case PosBuilderAtom(atom) => new BuilderCollection[Atom, Atom](head, Set[Atom](atom), Set[Atom]())
  //    case NegBuilderAtom(atom) => new BuilderCollection[Atom, Atom](head, Set[Atom](), Set[Atom](atom))
  //  }
}

object AspProgramBuilder {
  def rule(rule: AspRule) = new AspProgramBuilder(Set(rule))

  def apply(atomsToRules: PartialFunction[Seq[Atom], Set[AspRule]]): AspProgram = {
    val atoms = Stream.iterate(0)(x => x + 1).map(x => Atom("atom" + x))

    val rules = atomsToRules(atoms)

    AspProgram(rules.toList)
  }
}

class AspProgramBuilder(rules: Set[AspRule]) {
  def apply(rule: AspRule) = new AspProgramBuilder(rules + rule)

  def rule(rule: AspRule) = new AspProgramBuilder(rules + rule)

  def toProgram = new AspProgram(rules.toList)
}
