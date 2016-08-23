package core

import core.lars.Time

/**
  * Created by FM on 17.06.16.
  */
case class AtomModification(atom: Atom) {

  def apply(time: Time) = PinnedAtom(atom, time)

  def apply(arguments: List[Argument]): Atom = {
    val baseAtom: (Predicate, Seq[Argument]) = atom match {
      case aa: AtomWithArgument => (aa.predicate, aa.arguments)
      case a: Predicate => (a, Seq())
      case _ => (atom.predicate, Seq())
    }

    Atom(baseAtom._1, baseAtom._2 ++ arguments)
  }

  def apply(arguments: Argument*): Atom = this.apply(arguments.toList)

  // TODO: include arguments in atom-name?
  def asTupleReference(position: Long): GroundAtomWithArguments = GroundAtomWithArguments(Predicate(atom.predicate.toString + "_TUPLE"), Seq(Value(position)))

  def asFluentReference(): AtomWithArgument = {
    val arguments = atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }
    AtomWithArgument(Predicate(atom.predicate.toString + "_FLUENT"), arguments)
  }
}
