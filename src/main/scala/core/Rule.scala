package core

class RuleBuilder(bodyPos: Set[Atom] = Set(), bodyNeg: Set[Atom] = Set()) {
  def pos(atoms: Atom*) = new RuleBuilder(bodyPos ++ atoms, bodyNeg)

  def neg(atoms: Atom*) = new RuleBuilder(bodyPos, bodyNeg ++ atoms)

  def head(head: Atom) = new UserDefinedRule(bodyPos, bodyNeg, head)
}

object ConstraintBuilder {
  implicit def toRule(builder: ConstraintBuilder): Rule = new UserDefinedRule(builder.bodyPos, builder.bodyNeg, Falsum)
}

class ConstraintBuilder(val bodyPos: Set[Atom] = Set(),val bodyNeg: Set[Atom] = Set()) {
  def pos(atoms: Atom*) = new ConstraintBuilder(bodyPos ++ atoms, bodyNeg)

  def neg(atoms: Atom*) = new ConstraintBuilder(bodyPos, bodyNeg ++ atoms)
}

object Fact {
  def apply(head: Atom) = Rule.fact(head)
}

object Constraint {
  def pos(atoms: Atom*) = new ConstraintBuilder(atoms.toSet)

  def neg(atoms: Atom*) = new ConstraintBuilder(Set(), atoms.toSet)
}

object Rule {
  def pos(atoms: Atom*) = new RuleBuilder(atoms.toSet)

  def neg(atoms: Atom*) = new RuleBuilder(Set(), atoms.toSet)

  def fact(head: Atom) = new UserDefinedRule(Set(), Set(), head)
}

sealed trait Rule {
  val pos: Set[Atom]
  val neg: Set[Atom]
  val head: Atom

  val body = pos union neg
  val atoms = body + head
}

/**
  * Created by hb on 12/22/15.
  */
case class UserDefinedRule(pos: Set[Atom], neg: Set[Atom], head: Atom) extends Rule

case class RuleFromBacktracking(pos: Set[Atom], neg: Set[Atom], head: Atom) extends Rule