package core.lars

/**
  * Created by FM on 01.05.16.
  */
case class Rule(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom] = Set()) {
  val body = pos ++ neg
}

object Fact {
  def apply(head: HeadAtom) = Rule(head, Set())
}

case class Program(rules: Seq[Rule])

object Program {
  // apply won't work as we have the same signature as the case class :-/
  def from(rules: Rule*): Program = Program(rules)
}