package core.lars

import core.{Fact, Rule}

/**
  * Created by FM on 01.05.16.
  */
case class LarsRule(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom] = Set()) extends Rule[HeadAtom, ExtendedAtom]

case class LarsFact(head: HeadAtom) extends Fact[HeadAtom, ExtendedAtom]

case class LarsProgram(rules: Seq[LarsRule])

object LarsProgram {
  // apply won't work as we have the same signature as the case class :-/
  def from(rules: LarsRule*): LarsProgram = LarsProgram(rules)
}