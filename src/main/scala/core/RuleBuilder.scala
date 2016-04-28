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

object ConsequencesBuilder {

  implicit def rule(entailsBuilder: ConsequencesBuilder): Rule = {
    new UserDefinedRule(entailsBuilder.head, entailsBuilder.positiveBody, entailsBuilder.negativeBody)
  }

}

object ConstraintBuilder {
  implicit def toRule(builder: ConstraintBuilder): Rule = new UserDefinedRule(Falsum, builder.bodyPos, builder.bodyNeg)
}

class ConstraintBuilder(val bodyPos: Set[Atom] = Set(), val bodyNeg: Set[Atom] = Set()) {
  def pos(atoms: Atom*) = new ConstraintBuilder(bodyPos ++ atoms, bodyNeg)

  def neg(atoms: Atom*) = new ConstraintBuilder(bodyPos, bodyNeg ++ atoms)
}

// use object :- {} for contraint
// :- p, not a
// investigate of writing a program with DSL is possible
// check if we can use new-line
class ConsequencesBuilder(val head: Atom, val positiveBody: Set[Atom] = Set(), val negativeBody: Set[Atom] = Set()) {

  def :-(atom: Atom) = {
    new ConsequencesBuilder(head, positiveBody + atom, negativeBody)
  }

  def and(atom: Atom) = {
    new ConsequencesBuilder(head, positiveBody + atom, negativeBody)
  }

  def not(atom: Atom) = {
    new ConsequencesBuilder(head, positiveBody, negativeBody + atom)
  }

}

class Builder(head: Atom) {
  def :-(items: Any*) = {
    val pos = items map {
      case atom: Atom => atom
    }
    val neg = items map {
      case NegBuilderAtom(atom) => atom
    }

    new UserDefinedRule(head, pos.toSet, neg.toSet)
  }
}

object :- {
  //  def apply(atom: Atom) = new BuilderHead(Falsum) :- PosBuilderAtom(atom)

  def apply(item: BuilderItem) = new BuilderHead(Falsum) :- item

  //  def apply(item: BuilderItem) = new BuilderHead(Falsum) :- item
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
  implicit def toBuilderItem(atom: Atom) = new PosBuilderAtom(atom)
}

sealed trait BuilderItem

case class PosBuilderAtom(atom: Atom) extends BuilderItem

case class NegBuilderAtom(atom: Atom) extends BuilderItem

object ProgramBuilder {
  def rule(rule: Rule) = new ProgramBuilder(Set(rule))

  def apply(atomsToRules: PartialFunction[Seq[Atom], Set[Rule]]): Program = {
    val atoms = Stream.iterate(0)(x => x + 1).map(x => Atom("atom" + x))

    val rules = atomsToRules(atoms)

    Program(rules.toList)
  }
}

class ProgramBuilder(rules: Set[Rule]) {
  def apply(rule: Rule) = new ProgramBuilder(rules + rule)

  def rule(rule: Rule) = new ProgramBuilder(rules + rule)

  def toProgram = new Program(rules.toList)
}
