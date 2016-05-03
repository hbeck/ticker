package core.lars

//trait Rule

case class Rule(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom] = Set())

object Fact {
  def apply(head: HeadAtom) = Rule(head, Set())
}



