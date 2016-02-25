package jtms


class JustificationBuilder(I: Set[Atom] = Set(), O: Set[Atom] = Set()) {
  def in(atoms: Atom*) = new JustificationBuilder(I ++ atoms, O)

  def out(atoms: Atom*) = new JustificationBuilder(I, O ++ atoms)

  def head(head: Atom) = new UserDefinedJustification(I, O, head)
}

object Premise {
  def apply(head: Atom) = Justification.premise(head)
}

object Justification {
  def in(atoms: Atom*) = new JustificationBuilder(atoms.toSet)

  def out(atoms: Atom*) = new JustificationBuilder(Set(), atoms.toSet)

  def premise(head: Atom) = new UserDefinedJustification(Set(), Set(), head)
}

sealed trait Justification {
  val I: Set[Atom]
  val O: Set[Atom]
  val head: Atom
}

/**
  * Created by hb on 12/22/15.
  */
case class UserDefinedJustification(I: Set[Atom], O: Set[Atom], head: Atom) extends Justification

case class JustificationFromBacktracking(I: Set[Atom], O: Set[Atom], head: Atom) extends Justification