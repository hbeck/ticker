package core.asp

import core.Atom

object AspFact {
  def apply(head: Atom) = AspRule.fact(head)
}

object AspRule {
  def pos(atoms: Atom*) = new RuleBuilder(atoms.toSet)

  def neg(atoms: Atom*) = new RuleBuilder(Set(), atoms.toSet)

  def fact(head: Atom) = UserDefinedAspRule(head, Set(), Set())

  def apply(head: Atom, pos: Set[Atom], neg: Set[Atom]) = UserDefinedAspRule(head, pos, neg)

  def apply(head: Atom) = UserDefinedAspRule(head, Set(), Set())

  def apply(head: Atom, pos: Atom) = UserDefinedAspRule(head, Set(pos), Set())

  def apply(head: Atom, pos: Set[Atom]) = UserDefinedAspRule(head, pos, Set())

}

// TODO: discuss if sealed is needed (removed beacuse of PinnedRule)
trait AspRule {

  val pos: Set[Atom]
  val neg: Set[Atom]
  val head: Atom

  lazy val body = pos union neg
  lazy val atoms = body + head

  def isFact: Boolean = pos.isEmpty && neg.isEmpty

  def ==(other: AspRule): Boolean = {
    if (this.head != other.head) return false
    if (this.pos != other.pos) return false
    if (this.neg != other.neg) return false
    true
  }

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[AspRule]) {
      return false
    }
    val r = other.asInstanceOf[AspRule]
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

/**
  * Created by hb on 12/22/15.
  */
case class UserDefinedAspRule(head: Atom, pos: Set[Atom], neg: Set[Atom]) extends AspRule

case class AspRuleFromBacktracking(pos: Set[Atom], neg: Set[Atom], head: Atom) extends AspRule {
  override def toString = {
    super.toString.replaceAll("<-", "<--")
  }
}

object AspRuleFromBacktracking {
  def apply(pos: scala.collection.mutable.Set[Atom], neg: scala.collection.mutable.Set[Atom], head: Atom): AspRuleFromBacktracking = {
    AspRuleFromBacktracking(pos.toSet, neg.toSet, head)
  }
}