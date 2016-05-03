package core.lars

import core.Atom

/**
  * Created by FM on 03.05.16.
  */
/**
  * Created by FM on 27.02.16.
  */
class LarsRuleBuilder(bodyPos: Set[ExtendedAtom] = Set(), bodyNeg: Set[ExtendedAtom] = Set()) {
  def pos(atoms: ExtendedAtom*) = new LarsRuleBuilder(bodyPos ++ atoms, bodyNeg)

  def neg(atoms: ExtendedAtom*) = new LarsRuleBuilder(bodyPos, bodyNeg ++ atoms)

  def head(head: HeadAtom) = new Rule(head, bodyPos, bodyNeg)
}


class LarsBuilderHead(val head: HeadAtom) {
  def <=(item: BuilderItem) = item match {
    case PosBuilderAtom(atom) => new BuilderCollection(head, Set(atom))
    case NegBuilderAtom(atom) => new BuilderCollection(head, Set(), Set(atom))
  }
}

class BuilderCollection(val head: HeadAtom, val positiveBody: Set[ExtendedAtom] = Set(), val negativeBody: Set[ExtendedAtom] = Set()) {
  def and(builderItem: BuilderItem) = builderItem match {
    case PosBuilderAtom(atom) => new BuilderCollection(head, positiveBody + atom, negativeBody)
    case NegBuilderAtom(atom) => new BuilderCollection(head, positiveBody, negativeBody + atom)
  }
}

object BuilderCollection {
  implicit def toRule(builder: BuilderCollection): Rule = Rule(builder.head, builder.positiveBody, builder.negativeBody)
}

object not {
  def apply(atom: ExtendedAtom): NegBuilderAtom = NegBuilderAtom(atom)

  def apply(posBuilderAtom: PosBuilderAtom): NegBuilderAtom = NegBuilderAtom(posBuilderAtom.atom)
}

object BuilderItem {
  implicit def toBuilderItem(atom: ExtendedAtom): PosBuilderAtom = new PosBuilderAtom(atom)
}

sealed trait BuilderItem

case class PosBuilderAtom(atom: ExtendedAtom) extends BuilderItem

case class NegBuilderAtom(atom: ExtendedAtom) extends BuilderItem

object LarsProgramBuilder {
  def rule(rule: Rule) = new LarsProgramBuilder(Set(rule))

  def apply(atomsToRules: PartialFunction[Seq[Atom], Set[Rule]]): Program = {
    val atoms = Stream.iterate(0)(x => x + 1).map(x => Atom("atom" + x))

    val rules = atomsToRules(atoms)

    Program(rules.toSet)
  }
}

class LarsProgramBuilder(rules: Set[Rule]) {
  def apply(rule: Rule) = new LarsProgramBuilder(rules + rule)

  def rule(rule: Rule) = new LarsProgramBuilder(rules + rule)

  def toProgram = new Program(rules.toSet)
}
