package core

import core.lars.Time

/**
  * Created by FM on 17.06.16.
  */
case class AtomModification(atom: Atom) {
  def apply(time: Time) = PinnedAtom(atom, time)


  def apply(arguments: List[Argument]): AtomWithArgument = {
    val otherArguments: Seq[Argument] = atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Predicate => Seq()
      case _ => Seq()
    }

    val combinedArguments = otherArguments ++ arguments

    // TODO: use some real pattern matching
    //    combinedArguments match {
    //      case onlyValues: Seq[Value] => GroundAtomWithArguments(atom, onlyValues)
    //      case _ => AtomWithArguments(atom, combinedArguments)
    //    }
    combinedArguments.forall(_.isInstanceOf[Value]) match {
      case true => GroundAtomWithArguments(atom, combinedArguments.map(_.asInstanceOf[Value]).toList)
      case false => NonGroundAtom(atom, combinedArguments)
    }
  }

  def apply(arguments: Argument*): AtomWithArgument = this.apply(arguments.toList)

  def asTupleReference(position: Long): GroundAtomWithArguments = GroundAtomWithArguments(Atom(atom.toString + "_TUPLE"), Seq(Value(position)))
}
