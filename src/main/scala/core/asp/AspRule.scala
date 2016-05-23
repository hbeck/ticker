package core.asp

import core.Atom

object AspFact {
  def apply[TAtom <:Atom](head: TAtom) = AspRule.fact(head)
}

object AspRule {
  def pos[TAtom <:Atom](atoms: TAtom*) = new RuleBuilder(atoms.toSet)

  def neg[TAtom <:Atom](atoms: TAtom*) = new RuleBuilder(Set(), atoms.toSet)

  def fact[TAtom <:Atom](head: TAtom) = UserDefinedAspRule(head, Set(), Set())

  def apply[TAtom <:Atom](head: TAtom, pos: Set[TAtom], neg: Set[TAtom]) = UserDefinedAspRule(head, pos, neg)

  def apply[TAtom <:Atom](head: TAtom) = UserDefinedAspRule(head, Set(), Set())

  def apply[TAtom <:Atom](head: TAtom, pos: TAtom) = UserDefinedAspRule(head, Set(pos), Set())

  def apply[TAtom <:Atom](head: TAtom, pos: Set[TAtom]) = UserDefinedAspRule(head, pos, Set())

}

// TODO: discuss if sealed is needed (removed beacuse of PinnedRule)
trait AspRuleT[TAtom] {

  val pos: Set[TAtom]
  val neg: Set[TAtom]
  val head: TAtom

  lazy val body = pos union neg
  lazy val atoms = body + head

  def isFact: Boolean = pos.isEmpty && neg.isEmpty

  def ==(other: AspRuleT[TAtom]): Boolean = {
    if (this.head != other.head) return false
    if (this.pos != other.pos) return false
    if (this.neg != other.neg) return false
    true
  }

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[AspRuleT[TAtom]]) {
      return false
    }
    val r = other.asInstanceOf[AspRuleT[TAtom]]
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
case class UserDefinedAspRule[TAtom <:Atom](head: TAtom, pos: Set[TAtom], neg: Set[TAtom]) extends AspRuleT[TAtom]

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