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

  def :-(atom: Atom) = new BuilderCollection(head, Set(atom), Set[Atom]())

  def :-(notAtom: not) = new BuilderCollection(head, Set(), Set(notAtom.atom))
}


object AspProgramBuilder {
  def rule(rule: AspRule) = new AspProgramBuilder(Set(rule))

  def apply(atomsToRules: PartialFunction[Seq[Atom], Set[AspRule]]): AspProgram2 = {
    val atoms = Stream.iterate(0)(x => x + 1).map(x => Atom("atom" + x))

    val rules = atomsToRules(atoms)

    AspProgram(rules.toList)
  }
}

class AspProgramBuilder(rules: Set[AspRule]) {
  def apply(rule: AspRule) = new AspProgramBuilder(rules + rule)

  def rule(rule: AspRule) = new AspProgramBuilder(rules + rule)

  def toProgram = AspProgram(rules.toList)
}
