package core.lars

import core.{Atom,Fact}

/**
  * Created by FM on 01.05.16.
  */
object LarsRule {
  def apply(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]) = UserDefinedLarsRule(head, pos, neg)
}

case class UserDefinedLarsRule(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom] = Set()) extends LarsRule {

  override lazy val atoms: Set[ExtendedAtom] = pos union neg + head

  override def from(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): UserDefinedLarsRule = {
    UserDefinedLarsRule(head,pos,neg)
  }
}

case class LarsFact(head: HeadAtom) extends Fact[HeadAtom, ExtendedAtom] {

  override lazy val atoms: Set[ExtendedAtom] = Set[ExtendedAtom](head)

  override def from(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): LarsFact = {
    LarsFact(head)
  }
}

case class BasicLarsFact(head: Atom) extends Fact[Atom, ExtendedAtom] {

  override lazy val atoms: Set[ExtendedAtom] = Set[ExtendedAtom](head)

  override def from(head: Atom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): BasicLarsFact = {
    BasicLarsFact(head)
  }
}

case class LarsProgram(rules: Seq[LarsRule]) {
  lazy val atoms: Set[ExtendedAtom] = rules flatMap (r => r.body + r.head) toSet
  override def toString(): String = {
    val sb = new StringBuilder
    sb.append("{")
    if (rules.nonEmpty) {
      sb.append(" ").append(rules.head)
      if (rules.size>1) {
        rules.tail foreach (sb.append("; ").append(_))
      }
      sb.append(" ")
    }
    sb.append("}")
    sb.toString
  }
  def ==(other: LarsProgram): Boolean = {
    this.rules.toSet == other.rules.toSet
  }
}

object LarsProgram {
  // apply won't work as we have the same signature as the case class :-/
  def from(rules: LarsRule*): LarsProgram = LarsProgram(rules)
}