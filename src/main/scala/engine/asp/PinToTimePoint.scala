package engine.asp

import core.Atom
import core.asp._
import core.lars.TimePoint
import engine._

/**
  * Created by FM on 13.05.16.
  */

// TODO: remove this class?
case class PinToTimePoint(timePoint: TimePoint) {

  def atoms(atoms: Set[Atom]): PinnedStream = {
    atoms map (apply(_))
  }

  def apply(atom: Atom): PinnedFact = {
    AspFact(atom(timePoint))
  }
}
