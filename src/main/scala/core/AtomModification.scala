package core

/**
  * Created by FM on 17.06.16.
  *
  * helper class for algorithmic issues;
  * intended not to confuse more generic use cases
  * of Atom
  */
case class AtomModification(atom: Atom) {

  def appendArguments(arguments: Seq[Argument]): AtomWithArguments = {
    atom match {
      case p:PredicateAtom => AtomWithArguments(p.predicate,arguments)
      case a:AtomWithArguments => AtomWithArguments(a.predicate,a.arguments ++ arguments)
    }
  }

  def asTupleReference(position: Long) = {
    GroundAtomWithArguments(Predicate(atom.predicate.toString + "_TUPLE"), Seq(IntValue(position.toInt)))
  }

  def asCountReference(time: Argument, count: Argument): Atom = AtomWithArguments(Predicate("cnt_" + atom.predicate.caption), appendArguments(time, count))

  def asSpecificCountReference(time: Argument, count: Argument): Atom = AtomWithArguments(Predicate("cnt_specific_" + atom.predicate.caption), appendArguments(time, count))

  def asAtReference(time: Argument): Atom = AtomWithArguments(Predicate("at_" + atom.predicate.caption), appendArguments(time))

  def arguments(): Seq[Argument] = atom match {
    case aa: AtomWithArguments => aa.arguments
    case _ => Seq()
  }

  private def appendArguments(arguments: Argument*): Seq[Argument] = Atom.unapply(atom).getOrElse(Seq()) ++ arguments

}
