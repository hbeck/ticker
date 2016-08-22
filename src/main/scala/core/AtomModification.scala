package core

import core.lars.Time

/**
  * Created by FM on 17.06.16.
  */
case class AtomModification(atom: Atom) {

  def apply(time: Time) = PinnedAtom(atom, time)

  def apply(arguments: List[Argument]): AtomWithArgument = {
    val baseAtom: (Predicate, Seq[Argument]) = atom match {
      case aa: AtomWithArgument => (aa.predicate, aa.arguments)
      case a: Predicate => (a, Seq())
      case _ => (atom.predicate, Seq())
    }

    val combinedArguments = baseAtom._2 ++ arguments

    combinedArguments.forall(_.isInstanceOf[Value]) match {
      case true => GroundAtomWithArguments(baseAtom._1, combinedArguments.map(_.asInstanceOf[Value]).toList)
      case false => NonGroundAtom(baseAtom._1, combinedArguments)
    }
  }

  def apply(arguments: Argument*): AtomWithArgument = this.apply(arguments.toList)

  def asTupleReference(position: Long): GroundAtomWithArguments = GroundAtomWithArguments(Predicate(atom.toString + "_TUPLE"), Seq(Value(position)))
}
