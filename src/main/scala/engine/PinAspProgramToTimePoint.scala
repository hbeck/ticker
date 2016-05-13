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


    val atoms = (dataStream map (x => atomAtT(x.time, x.atoms.head))) + nowAtT
    AspProgramAtTimePoint(program, atoms, timePoint)
  }

  def atomAtT(time: TimePoint, atom: Atom): AspRule = {
    AspFact(atom(time.timePoint))
  }


}

case class AspProgramAtTimePoint(baseProgram: AspProgram, pinnedAtoms: Set[AspRule], timePoint: TimePoint) extends AspProgram {
  val rules: Seq[AspRule] = baseProgram.rules ++ pinnedAtoms
}