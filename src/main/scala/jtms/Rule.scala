package jtms

class RuleBuilder(I: Set[Atom] = Set(), O: Set[Atom] = Set()) {
  def in(atoms: Atom*) = new RuleBuilder(I ++ atoms, O)

  def out(atoms: Atom*) = new RuleBuilder(I, O ++ atoms)

  def head(head: Atom) = new UserDefinedRule(I, O, head)
}

object Premise {
  def apply(head: Atom) = Rule.premise(head)
}

object Rule {
  def in(atoms: Atom*) = new RuleBuilder(atoms.toSet)

  def out(atoms: Atom*) = new RuleBuilder(Set(), atoms.toSet)

  def premise(head: Atom) = new UserDefinedRule(Set(), Set(), head)
}

sealed trait Rule {
  val I: Set[Atom]
  val O: Set[Atom]
  val head: Atom

  val atoms = I union O + head
}

/**
  * Created by hb on 12/22/15.
  */
case class UserDefinedRule(I: Set[Atom], O: Set[Atom], head: Atom) extends Rule

case class RuleFromBacktracking(I: Set[Atom], O: Set[Atom], head: Atom) extends Rule