package core.lars

import core.Atom
import engine.Time

/**
  * Created by FM on 01.05.16.
  */
case class AtAtom(val time: Time, val atom: Atom) extends HeadAtom
