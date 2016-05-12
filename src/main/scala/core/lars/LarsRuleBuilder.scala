package core.lars

import core._

/**
  * Created by FM on 03.05.16.
  */
class LarsBuilderHead(val head: HeadAtom) {
  def <=(extendedAtom: ExtendedAtom) = new BuilderCollection(head, Set(extendedAtom), Set[ExtendedAtom]())

  def <=(notAtom: not) = new BuilderCollection(head, Set(), Set(notAtom.atom))
}


object LarsProgramBuilder {
  def rule(rule: Rule) = new LarsProgramBuilder(Seq(rule))

  def apply(atomsToRules: PartialFunction[Seq[Atom], Seq[Rule]]): Program = {
    val atoms = Stream.iterate(0)(x => x + 1).map(x => Atom("atom" + x))

    val rules = atomsToRules(atoms)

    Program(rules)
  }
}

class LarsProgramBuilder(rules: Seq[Rule]) {
  def apply(rule: Rule) = new LarsProgramBuilder(rules :+ rule)

  def rule(rule: Rule) = new LarsProgramBuilder(rules :+ rule)

  def toProgram = new Program(rules)
}
