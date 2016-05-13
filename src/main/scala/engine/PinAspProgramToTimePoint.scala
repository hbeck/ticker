package engine

import core.Atom
import core.asp.{AspRule, AspFact, AspProgram}
import core.lars.{TimePoint, Time}

/**
  * Created by FM on 13.05.16.
  */
object PinAspProgramToTimePoint {
  def apply(program: AspProgram, dataStream: Stream, timePoint: TimePoint) = {
    val nowAtT = atomAtT(timePoint, now)

    val atoms = dataStream flatMap atomsAtT

    AspProgramAtTimePoint(program, atoms + nowAtT, timePoint)
  }

  def atomsAtT(streamEntry: StreamEntry): Set[AspRule] = atomsAtT(streamEntry.time, streamEntry.atoms)

  def atomsAtT(timePoint: TimePoint, atoms: Set[Atom]): Set[AspRule] = {
    atoms map (atomAtT(timePoint, _))
  }

  def atomAtT(time: TimePoint, atom: Atom): AspRule = {
    AspFact(atom(time.timePoint))
  }


}

case class AspProgramAtTimePoint(baseProgram: AspProgram, pinnedAtoms: Set[AspRule], timePoint: TimePoint) extends AspProgram {
  val rules: Seq[AspRule] = baseProgram.rules ++ pinnedAtoms
}