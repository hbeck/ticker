package engine

import core.Atom
import core.asp.{AspFact, AspProgram}
import core.lars.{TimePoint, Time}

/**
  * Created by FM on 13.05.16.
  */
object TransformAspToTimePoint {
  def apply(program: AspProgram, dataStream: Stream, timePoint: TimePoint) = {
    val nowAtT = atomAtT(timePoint, now)


    val atoms = (dataStream map (x => atomAtT(x.time, x.atoms.head))) + nowAtT
    AspProgramAtTimePoint(AspProgram(program.rules ++ atoms), timePoint)
  }

  def atomAtT(time: TimePoint, atom: Atom) = {
    AspFact(atom(time.timePoint))
  }


}

// TODO PinAspProgramToTimePoint
case class AspProgramAtTimePoint(program: AspProgram, timePoint: TimePoint)