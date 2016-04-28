package core

object Fact {
  def apply(head: Atom) = Rule.fact(head)
}

object Rule {
  def pos(atoms: Atom*) = new RuleBuilder(atoms.toSet)

  def neg(atoms: Atom*) = new RuleBuilder(Set(), atoms.toSet)

  def fact(head: Atom) = UserDefinedRule(head, Set(), Set())

  def apply(head: Atom, pos:Set[Atom], neg: Set[Atom]) = UserDefinedRule(head, pos, neg)
  def apply(head: Atom) = UserDefinedRule(head, Set(), Set())
  def apply(head: Atom, pos: Atom) = UserDefinedRule(head, Set(pos), Set())
  def apply(head: Atom, pos: Set[Atom]) = UserDefinedRule(head, pos, Set())

}

sealed trait Rule {
  val pos: Set[Atom]
  val neg: Set[Atom]
  val head: Atom

  val body = pos union neg
  val atoms = body + head

  def ==(other: Rule): Boolean = {
    if (this.head != other.head) return false
    if (this.pos != other.pos) return false
    if (this.neg != other.neg) return false
    true
  }

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[Rule]) {
      return false
    }
    val r = other.asInstanceOf[Rule]
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
case class UserDefinedRule(head: Atom, pos: Set[Atom], neg: Set[Atom]) extends Rule

case class RuleFromBacktracking(pos: Set[Atom], neg: Set[Atom], head: Atom) extends Rule {
  override def toString = {
    super.toString.replaceAll("<-","<--")
  }
}

object RuleFromBacktracking {
  def apply (pos: scala.collection.mutable.Set[Atom], neg: scala.collection.mutable.Set[Atom], head: Atom): RuleFromBacktracking = {
    RuleFromBacktracking(pos.toSet,neg.toSet,head)
  }
}