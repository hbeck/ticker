package core.asp

import core.{Atom, Fact, Rule}

sealed trait AspRule[TAtom <: Atom] extends Rule[TAtom, TAtom] {

  // TODO: can we move this into Rule[..]?
  lazy val atoms = body + head
  // TODO: can we move this into Rule[..]?
  lazy val isGround: Boolean = atoms forall (_.isGround)

  override def toString = {
    val sb = new StringBuilder
    sb.append(head).append(" <- ")
    if (!pos.isEmpty) {
      sb.append(pos)
    }
    if (!neg.isEmpty) {
      if (!pos.isEmpty) {
        sb.append(", ")
      }
      sb.append("not ").append(neg)
    }
    sb.toString
  }
}

trait AspFact[TAtom <: Atom] extends AspRule[TAtom] with Fact[TAtom, TAtom]


object AspRule {

  def pos[TAtom <: Atom](atoms: TAtom*) = new RuleBuilder(atoms.toSet)

  def neg[TAtom <: Atom](atoms: TAtom*) = new RuleBuilder(Set(), atoms.toSet)

  def fact[TAtom <: Atom](head: TAtom) = AspFact(head)

  def apply[TAtom <: Atom](head: TAtom, pos: Set[TAtom], neg: Set[TAtom]) = UserDefinedAspRule(head, pos, neg)

  def apply[TAtom <: Atom](head: TAtom): AspFact[TAtom] = AspFact(head)

  def apply[TAtom <: Atom](head: TAtom, pos: TAtom) = UserDefinedAspRule(head, Set(pos), Set())

  def apply[TAtom <: Atom, TPos <: TAtom](head: TAtom, pos: Set[TPos]) = UserDefinedAspRule(head, pos.toSet[TAtom], Set[TAtom]())

}

case class UserDefinedAspRule[TAtom <: Atom](head: TAtom, pos: Set[TAtom], neg: Set[TAtom]) extends AspRule[TAtom]{
//  private lazy val precomputedHash = super.hashCode()
//
//  override def hashCode(): Int = precomputedHash
}

case class AspRuleFromBacktracking(pos: Set[Atom], neg: Set[Atom], head: Atom) extends NormalRule {
  override def toString = {
    super.toString.replaceAll("<-", "<--")
  }
}

object AspRuleFromBacktracking {
  def apply(pos: scala.collection.mutable.Set[Atom], neg: scala.collection.mutable.Set[Atom], head: Atom): AspRuleFromBacktracking = {
    AspRuleFromBacktracking(pos.toSet, neg.toSet, head)
  }
}

object AspFact {
  def apply[TAtom <: Atom](fact: TAtom): AspFact[TAtom] = UserDefinedAspFact(fact)
}

case class UserDefinedAspFact[TAtom <: Atom](head: TAtom) extends AspFact[TAtom] {
  override def toString = {
    head.toString
  }
}