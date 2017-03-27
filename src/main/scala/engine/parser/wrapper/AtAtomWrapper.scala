package engine.parser.wrapper

import core.lars.AtAtom

/**
  * Created by et on 21.03.17.
  */
case class AtAtomWrapper(not: Option[Any], atAtom: AtomWrapper, time: String) extends AtomTrait {

//  val atAtom: AtAtom = create(time,atAtom)
  override val neg: Boolean = not.isDefined
  val atom: AtAtom = create(time,atAtom)

  def create(time: String, atom: AtomWrapper): AtAtom = {
    //TODO check if time is actually something useful
    AtAtom(time.toInt,atom.atom)
  }

}
