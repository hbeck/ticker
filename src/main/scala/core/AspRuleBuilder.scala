package core

import scala.language.implicitConversions

/**
  * Created by FM on 27.02.16.
  */
class RuleBuilder(bodyPos: Set[Atom] = Set(), bodyNeg: Set[Atom] = Set()) {
  def pos(atoms: Atom*) = new RuleBuilder(bodyPos ++ atoms, bodyNeg)

  def neg(atoms: Atom*) = new RuleBuilder(bodyPos, bodyNeg ++ atoms)

  def head(head: Atom) = new UserDefinedRule(head, bodyPos, bodyNeg)
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
  implicit def toRule(builder: BuilderCollection): Rule = new UserDefinedRule(builder.head, builder.positiveBody, builder.negativeBody)
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
  def rule(rule: Rule) = new AspProgramBuilder(Set(rule))

  def apply(atomsToRules: PartialFunction[Seq[Atom], Set[Rule]]): Program = {
    val atoms = Stream.iterate(0)(x => x + 1).map(x => Atom("atom" + x))

    val rules = atomsToRules(atoms)

    Program(rules.toList)
  }
}

class AspProgramBuilder(rules: Set[Rule]) {
  def apply(rule: Rule) = new AspProgramBuilder(rules + rule)

  def rule(rule: Rule) = new AspProgramBuilder(rules + rule)

  def toProgram = new Program(rules.toList)
}
