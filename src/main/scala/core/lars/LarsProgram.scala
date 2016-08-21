package core.lars

import core.Fact

/**
  * Created by FM on 01.05.16.
  */
object LarsRule {
  def apply(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]) = UserDefinedLarsRule(head, pos, neg)
}

case class UserDefinedLarsRule(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom] = Set()) extends LarsRule

case class LarsFact(head: HeadAtom) extends Fact[HeadAtom, ExtendedAtom]

case class LarsProgram(rules: Seq[LarsRule]) {
  lazy val atoms: Set[ExtendedAtom] = rules flatMap (r => r.body + r.head) toSet
}

object LarsProgram {
  // apply won't work as we have the same signature as the case class :-/
  def from(rules: LarsRule*): LarsProgram = LarsProgram(rules)
}