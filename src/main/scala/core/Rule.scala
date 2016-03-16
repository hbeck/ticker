package core

class RuleBuilder(bodyPos: Set[Atom] = Set(), bodyNeg: Set[Atom] = Set()) {
  def pos(atoms: Atom*) = new RuleBuilder(bodyPos ++ atoms, bodyNeg)

  def neg(atoms: Atom*) = new RuleBuilder(bodyPos, bodyNeg ++ atoms)

  def head(head: Atom) = new UserDefinedRule(bodyPos, bodyNeg, head)
}

object Fact {
  def apply(head: Atom) = Rule.fact(head)
}

object Rule {
  def pos(atoms: Atom*) = new RuleBuilder(atoms.toSet)

  def neg(atoms: Atom*) = new RuleBuilder(Set(), atoms.toSet)

  def fact(head: Atom) = UserDefinedRule(Set(), Set(), head)

  def apply(head: Atom, pos:Set[Atom], neg: Set[Atom]) = UserDefinedRule(pos,neg,head)
  def apply(head: Atom) = UserDefinedRule(Set(),Set(),head)
  def apply(head: Atom, pos: Atom) = UserDefinedRule(Set(pos),Set(),head)
  def apply(head: Atom, pos: Set[Atom]) = UserDefinedRule(pos,Set(),head)

//  def apply(head: Atom, pos: Option[Set[Atom]], neg: Option[Set[Atom]]) = {
//    val p = pos match { case Some(set) => set; case None => Set[Atom]() }
//    val n = neg match { case Some(set) => set; case None => Set[Atom]() }
//    apply(head,p,n)
//  }
}

sealed trait Rule {
  val pos: Set[Atom]
  val neg: Set[Atom]
  val head: Atom //TODO later: Option[Atom] for constraints

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
}

/**
  * Created by hb on 12/22/15.
  */
//TODO (hb) following order is better: (head, pos, neg)
case class UserDefinedRule(pos: Set[Atom], neg: Set[Atom], head: Atom) extends Rule {
//  override def toString = {
//    val sb = new StringBuilder()
//    sb.append(head)
//    if (!pos.isEmpty && !neg.isEmpty) {
//      sb.append(" :- ")
//      for (a <- pos) {
//        sb.append(a).append(", ")
//      }
//      for (a <- neg) {
//        sb.append("not ").append(a).append(", ")
//      }
//    }
//    sb.substring(0,sb.length-1)
//  }
}

case class RuleFromBacktracking(pos: Set[Atom], neg: Set[Atom], head: Atom) extends Rule

object RuleFromBacktracking {
  def apply (pos: scala.collection.mutable.Set[Atom], neg: scala.collection.mutable.Set[Atom], head: Atom): RuleFromBacktracking = {
    RuleFromBacktracking(pos.toSet,neg.toSet,head)
  }
}