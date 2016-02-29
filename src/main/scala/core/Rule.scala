package core

class RuleBuilder(pos: Set[Atom] = Set(), neg: Set[Atom] = Set()) {
  def pos(atoms: Atom*) = new RuleBuilder(pos ++ atoms, neg)

  def neg(atoms: Atom*) = new RuleBuilder(pos, neg ++ atoms)

  def head(head: Atom) = new UserDefinedRule(pos, neg, head)
}

object Premise {
  def apply(head: Atom) = Rule.premise(head)
}

object Rule {
  def pos(atoms: Atom*) = new RuleBuilder(atoms.toSet)

  def neg(atoms: Atom*) = new RuleBuilder(Set(), atoms.toSet)

  def premise(head: Atom) = new UserDefinedRule(Set(), Set(), head)
}

sealed trait Rule {
  val pos: Set[Atom]
  val neg: Set[Atom]
  val head: Atom //TODO later: Option[Atom] for constraints

  val body = pos union neg
  val atoms = body + head
}

/**
  * Created by hb on 12/22/15.
  */
case class UserDefinedRule(pos: Set[Atom], neg: Set[Atom], head: Atom) extends Rule

case class RuleFromBacktracking(pos: Set[Atom], neg: Set[Atom], head: Atom) extends Rule