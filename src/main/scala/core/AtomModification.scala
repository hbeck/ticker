package core

import core.lars.Time

/**
  * Created by FM on 17.06.16.
  */
case class AtomModification(atom: Atom) {
  def apply(time: Time) = PinnedAtom(atom, time)


  def apply(arguments: Argument*): Atom = {
    val otherArguments: Seq[Argument] = atom match {
      case AtomWithArguments(_, args) => args
      case a: Atom => Seq()
    }
    AtomWithArguments(atom, otherArguments ++ arguments)
  }

  def ground(variable: Variable, value: String): Unit = {

    if (!value.head.isLower)
      throw new IllegalArgumentException("Cannot ground to " + value)

  }
}
