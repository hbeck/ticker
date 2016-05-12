package core.lars

import core.Atom
import engine.Time

/**
  * Created by FM on 01.05.16.
  */

// case class is not (technical) possible - use this workaround for now
// TODO: Discuss hierarchy / implementation
case class AtAtom(val time: Time, val atom: Atom) extends HeadAtom with ExtendedAtom
//
//
//object AtAtom {
//  def apply(time: Time, atom: Atom) = new AtAtom(time, atom)
//}