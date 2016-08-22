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

    val combinedArguments = baseAtom._2 ++ arguments

    combinedArguments.forall(_.isInstanceOf[Value]) match {
      case true => GroundAtom(baseAtom._1, combinedArguments.map(_.asInstanceOf[Value]).toList: _*)
      case false => NonGroundAtom(baseAtom._1, combinedArguments)
    }
  }

  def apply(arguments: Argument*): Atom = this.apply(arguments.toList)

  // TODO: include arguments in atom-name?
  def asTupleReference(position: Long): GroundAtomWithArguments = GroundAtomWithArguments(Predicate(atom.predicate.toString + "_TUPLE"), Seq(Value(position)))
}
