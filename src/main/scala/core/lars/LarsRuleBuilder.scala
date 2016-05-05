package core.lars

import core._

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
  def <= = new NotBuilderCollection(head, Set[ExtendedAtom](), Set[ExtendedAtom]())

  //  def <=(item: BuilderItem[ExtendedAtom]) = item match {
  //    case PosBuilderAtom(atom) => new BuilderCollection(head, Set[ExtendedAtom](atom),Set[ExtendedAtom]())
  //    case NegBuilderAtom(atom) => new BuilderCollection(head, Set[ExtendedAtom](), Set[ExtendedAtom](atom))
  //  }
}


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
