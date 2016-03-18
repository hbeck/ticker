package core

import scala.language.implicitConversions

/**
  * Created by FM on 27.02.16.
  */
class RuleBuilder(bodyPos: Set[Atom] = Set(), bodyNeg: Set[Atom] = Set()) {
  def pos(atoms: Atom*) = new RuleBuilder(bodyPos ++ atoms, bodyNeg)

  def neg(atoms: Atom*) = new RuleBuilder(bodyPos, bodyNeg ++ atoms)

  def head(head: Atom) = new UserDefinedRule(bodyPos, bodyNeg, head)
}

object ConsequencesBuilder {

  implicit def rule(entailsBuilder: ConsequencesBuilder): Rule = {
    new UserDefinedRule(entailsBuilder.positiveBody, entailsBuilder.negativeBody, entailsBuilder.head)
  }

}

object ConstraintBuilder {
  implicit def toRule(builder: ConstraintBuilder): Rule = new UserDefinedRule(builder.bodyPos, builder.bodyNeg, Falsum)
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

    new UserDefinedRule(pos.toSet, neg.toSet, head)
  }
}

object :- {
  //  def apply(atom: Atom) = new BuilderHead(Falsum) :- PosBuilderAtom(atom)

  def apply(item:BuilderItem)= new BuilderHead(Falsum) :- item

  //  def apply(item: BuilderItem) = new BuilderHead(Falsum) :- item
}

class BuilderHead(head: Atom) {
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


  implicit def toRule(): Rule = new UserDefinedRule(positiveBody, negativeBody, head)
}

object BuilderCollection {
  implicit def toRule(builder: BuilderCollection): Rule = new UserDefinedRule(builder.positiveBody, builder.negativeBody, builder.head)

  //  implicit def toRule(builder: BuilderCollection): Rule = new UserDefinedRule(builder.bodyPos, builder.bodyNeg, Falsum)
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
  def apply(rule: Rule) = {
    new ProgramBuilder(Set(rule))
  }

  //  def apply(rules: Rule*)={
  //    new ProgramBuilder(rules.toSet)
  //  }
  implicit def toProgramBuilder(rule: Rule) = ProgramBuilder(rule)

  def rule(rule: Rule) = ProgramBuilder(rule)

  def +(rule: Rule) = rule(rule)
}

class ProgramBuilder(rules: Set[Rule]) {
  def apply(rule: Rule) = new ProgramBuilder(rules + rule)

  def rule(rule: Rule) = new ProgramBuilder(rules + rule)

  def toProgram = new Program(rules.toList)

}

object AtomProgramBuilder {
  def foo(function: Function[Atom, Set[Rule]]) = {

  }
}
