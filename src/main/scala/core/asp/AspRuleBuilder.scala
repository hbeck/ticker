package core.asp

import core.Atom

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
  def :-(item: BuilderItem) = item match {
    case PosBuilderAtom(atom) => new BuilderCollection(head, Set(atom))
    case NegBuilderAtom(atom) => new BuilderCollection(head, Set(), Set(atom))
  }
}


class BuilderCollection(val head: Atom, val positiveBody: Set[Atom] = Set(), val negativeBody: Set[Atom] = Set()) {
  def and(builderItem: BuilderItem) = builderItem match {
    case PosBuilderAtom(atom) => new BuilderCollection(head, positiveBody + atom, negativeBody)
    case NegBuilderAtom(atom) => new BuilderCollection(head, positiveBody, negativeBody + atom)
  }
}

object BuilderCollection {
  implicit def toRule(builder: BuilderCollection): AspRule = new UserDefinedAspRule(builder.head, builder.positiveBody, builder.negativeBody)
}

object not {
  def apply(atom: Atom): NegBuilderAtom = NegBuilderAtom(atom)

  def apply(posBuilderAtom: PosBuilderAtom): NegBuilderAtom = NegBuilderAtom(posBuilderAtom.atom)
}

object BuilderItem {
  implicit def toBuilderItem(atom: Atom): PosBuilderAtom = new PosBuilderAtom(atom)
}

sealed trait BuilderItem

case class PosBuilderAtom(atom: Atom) extends BuilderItem

case class NegBuilderAtom(atom: Atom) extends BuilderItem

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
