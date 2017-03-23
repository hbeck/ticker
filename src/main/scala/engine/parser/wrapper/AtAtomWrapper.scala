package engine.parser.wrapper

import core.Predicate
import core.lars.AtAtom

/**
  * Created by et on 21.03.17.
  */
case class AtAtomWrapper(not: Option[Any], atom: AtomWrapper, time: String) extends AtomTrait{

  val atAtom: AtAtom = create(time,atom)
  val neg: Boolean = not.isDefined

  def create(time: String, atom: AtomWrapper): AtAtom = {
    //TODO check if time is actually something useful
    AtAtom(time.toInt,atom.atom)
  }

}
