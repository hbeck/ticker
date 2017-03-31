package engine.parser.factory

import core.lars.AtAtom

/**
  * Created by et on 21.03.17.
  */
case class AtAtomFactory(not: Option[Any], atAtom: AtomFactory, time: String) extends AtomTrait {

//  val atAtom: AtAtom = create(time,atAtom)
  override val neg: Boolean = not.isDefined
  val atom: AtAtom = create(time,atAtom)

  def create(time: String, atom: AtomFactory): AtAtom = {
    //TODO check if time is actually something useful
    AtAtom(time.toInt,atom.atom)
  }

}
