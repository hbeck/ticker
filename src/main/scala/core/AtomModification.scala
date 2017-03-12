package core

import core.lars.Time
import engine.asp.{cnt, now}

/**
  * Created by FM on 17.06.16.
  *
  * helper class for algorithmic issues;
  * intended not to confuse more generic use cases
  * of Atom
  */
case class AtomModification(atom: Atom) {

  def apply(time: Time): AtomWithArguments = {
    val timeAsArgument: Argument = time

    // TODO: for these special atoms we don't want them to be mapped as PinnedAtoms
    // because this would create predicate names like 'now_at'
    atom match {
      case `cnt` => this (timeAsArgument)
      case `now` => this (timeAsArgument)
      case _ => PinnedAtom(atom, time)
    }
  }

  def apply(arguments: List[Argument]): AtomWithArguments = {
    val (pred, atomArgs): (Predicate, Seq[Argument]) = atom match {
      case aa: AtomWithArguments => (aa.predicate, aa.arguments)
      case _ => (atom.predicate, Seq())
    }
    // TODO: is this cast needed?
    Atom(pred, atomArgs ++ arguments).asInstanceOf[AtomWithArguments]
  }

  def apply(arguments: Argument*): AtomWithArguments = this.apply(arguments.toList)

  def appendTimeAsNormalArgument(time: Time): AtomWithArguments = this.apply(time.asInstanceOf[Argument])

  def asTupleReference(position: Long) = {
    GroundAtomWithArguments(Predicate(atom.predicate.toString + "_TUPLE"), Seq(IntValue(position.toInt)))
  }

  def asCountReference(time: Argument, count: Argument): Atom = AtomWithArguments(Predicate("cnt_" + atom.predicate.caption), appendArguments(time, count))

  def asSpecificCountReference(time: Argument, count: Argument): Atom = AtomWithArguments(Predicate("cnt_specific_" + atom.predicate.caption), appendArguments(time, count))

  def asAtReference(time: Argument): Atom = AtomWithArguments(Predicate("at_" + atom.predicate.caption), appendArguments(time))

  def asFluentReference(): AtomWithArguments = {
    val arguments = atom match {
      case aa: AtomWithArguments => aa.arguments
      case a: Atom => Seq()
    }
    AtomWithArguments(Predicate(atom.predicate.toString + "_FLUENT"), arguments)
  }

  def arguments(): Seq[Argument] = atom match {
    case aa: AtomWithArguments => aa.arguments
    case _ => Seq()
  }

  private def appendArguments(arguments: Argument*): Seq[Argument] = Atom.unapply(atom).getOrElse(Seq()) ++ arguments

}
