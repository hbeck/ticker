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

  //TODO why do we have this in addition to Pinned{Time,At}Atom?
  def appendTimeAsArgument(time: Time): AtomWithArguments = {
    val timeAsArgument: Argument = time

    atom.predicate match {
      case `cnt` => appendArguments(List(timeAsArgument))
      case `now` => appendArguments(List(timeAsArgument))
      case _ => PinnedAtom.asPinnedAtAtom(atom, time)
    }
  }

  /* what is this
  def apply(arguments: List[Argument]): AtomWithArguments = {
    val (pred, atomArgs): (Predicate, Seq[Argument]) = atom match {
      case aa: AtomWithArguments => (aa.predicate, aa.arguments)
      case _ => (atom.predicate, Seq())
    }
    // TODO: is this cast needed?
    Atom(pred, atomArgs ++ arguments).asInstanceOf[AtomWithArguments]
  }
  */

  def apply(arguments: Argument*): AtomWithArguments = appendArguments(arguments.toList)

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
