package engine.asp.evaluation

import core.Atom
import core.asp._
import core.lars.TimePoint
import engine._
import engine.asp._

/**
  * Created by FM on 13.05.16.
  */
case class PinToTimePoint(timePoint: TimePoint) {
  def apply(dataStream: Stream): PinnedStream = {
    val nowAtT = apply(now)

    val pinnedAtoms = dataStream flatMap (x => PinToTimePoint(x.time).atoms(x.atoms))

    pinnedAtoms + nowAtT
  }

  def atoms(atoms: Set[Atom]): PinnedStream = {
    atoms map (apply(_))
  }

  def apply(atom: Atom): PinnedFact = {
    AspFact(atom(timePoint))
  }

  //  def apply(program: Seq[PinnedAspRule], dataStream: Stream): AspProgramAtTimePoint = {
  //    AspProgramAtTimePoint(program, apply(dataStream), timePoint)
  //  }
}
