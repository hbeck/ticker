package core

import core.lars.Time

/**
  * Created by FM on 17.06.16.
  *
  * helper class for algorithmic issues;
  * intended not to confuse more generic use cases
  * of Atom
  */
case class AtomModification(atom: Atom) {

  def apply(time: Time) = PinnedAtom(atom, time)

  def apply(arguments: List[Argument]): Atom = {
    val (pred, atomArgs): (Predicate, Seq[Argument]) = atom match {
      case aa: AtomWithArgument => (aa.predicate, aa.arguments)
      case _ => (atom.predicate, Seq())
    }
    Atom(pred, atomArgs ++ arguments)
  }

  def apply(arguments: Argument*): Atom = this.apply(arguments.toList)

  def asTupleReference(position: Long) = {
    GroundAtomWithArguments(Predicate(atom.predicate.toString + "_TUPLE"), Seq(IntValue(position.toInt)))
  }

  def asCountReference(time: Argument, count: Argument): Atom = AtomWithArgument(Predicate("cnt_" + atom.predicate.caption), appendArguments(time, count))

  def asSpecificCountReference(time: Argument, count: Argument): Atom = AtomWithArgument(Predicate("cnt_specific_" + atom.predicate.caption), appendArguments(time, count))

  def asAtReference(time: Argument): Atom = AtomWithArgument(Predicate("at_" + atom.predicate.caption), appendArguments(time))

  def asFluentReference(): AtomWithArgument = {
    val arguments = atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }
    AtomWithArgument(Predicate(atom.predicate.toString + "_FLUENT"), arguments)
  }

  def arguments(): Seq[Argument] = atom match {
    case aa: AtomWithArgument => aa.arguments
    case _ => Seq()
  }

  private def appendArguments(arguments: Argument*): Seq[Argument] = Atom.unapply(atom).getOrElse(Seq()) ++ arguments

}
