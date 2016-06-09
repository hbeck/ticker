package core.lars

/**
  * Created by FM on 01.05.16.
  */
case class LarsRule(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom] = Set()) {
  val body = pos ++ neg
}

object LarsFact {
  def apply(head: HeadAtom) = LarsRule(head, Set())
}

case class LarsProgram(rules: Seq[LarsRule])

object LarsProgram {
  // apply won't work as we have the same signature as the case class :-/
  def from(rules: LarsRule*): LarsProgram = LarsProgram(rules)
}