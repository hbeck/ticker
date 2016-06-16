package core.asp

import core.{Atom, Fact, Rule}

object AspRule {

  def pos[TAtom <: Atom](atoms: TAtom*) = new RuleBuilder(atoms.toSet)

  def neg[TAtom <: Atom](atoms: TAtom*) = new RuleBuilder(Set(), atoms.toSet)

  def fact[TAtom <: Atom](head: TAtom) = AspFact(head)

  def apply[TAtom <: Atom](head: TAtom, pos: Set[TAtom], neg: Set[TAtom]) = UserDefinedAspRule(head, pos, neg)

  def apply[TAtom <: Atom](head: TAtom): AspFact[TAtom] = AspFact(head)

  def apply[TAtom <: Atom](head: TAtom, pos: TAtom) = UserDefinedAspRule(head, Set(pos), Set())

  def apply[TAtom <: Atom](head: TAtom, pos: Set[TAtom]) = UserDefinedAspRule(head, pos, Set())

}

// TODO: discuss if sealed is needed (removed because of GroundedRule)
trait AspRule[TAtom <: Atom] extends Rule[TAtom, TAtom] {

  lazy val atoms = body + head

  def isFact: Boolean = pos.isEmpty && neg.isEmpty

  def isGround: Boolean = atoms forall (_.isGround)

  def ==(other: AspRule[TAtom]): Boolean = {
    if (this.head != other.head) return false
    if (this.pos != other.pos) return false
    if (this.neg != other.neg) return false
    true
  }

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[AspRule[TAtom]]) {
      return false
    }
    val r = other.asInstanceOf[AspRule[TAtom]]
    return this == r
  }

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

/**
  * Created by hb on 12/22/15.
  */
case class UserDefinedAspRule[TAtom <: Atom](head: TAtom, pos: Set[TAtom], neg: Set[TAtom]) extends AspRule[TAtom]

case class AspRuleFromBacktracking(pos: Set[Atom], neg: Set[Atom], head: Atom) extends NormalRule {
  override def toString = {
    super.toString.replaceAll("<-", "<--")
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

object AspRuleFromBacktracking {
  def apply(pos: scala.collection.mutable.Set[Atom], neg: scala.collection.mutable.Set[Atom], head: Atom): AspRuleFromBacktracking = {
    AspRuleFromBacktracking(pos.toSet, neg.toSet, head)
  }
}