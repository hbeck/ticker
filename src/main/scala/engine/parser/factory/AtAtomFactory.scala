package engine.parser.factory

import core.Variable
import core.lars.AtAtom

/**
  * Created by et on 21.03.17.
  */
case class AtAtomFactory(override val neg: Boolean, atAtom: AtomFactory, time: String) extends AtomTrait {

  lazy val atom: AtAtom = create(time,atAtom)

  def create(time: String, atom: AtomFactory): AtAtom = {
    if(time.forall(Character.isDigit)) return AtAtom(time.toInt, atom.atom)
    AtAtom(Variable(time),atom.atom)
  }
}
