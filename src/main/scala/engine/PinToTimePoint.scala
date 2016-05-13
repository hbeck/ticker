package engine

import core.Atom
import core.asp.{AspRule, AspFact, AspProgram}
import core.lars.{TimePoint, Time}

/**
  * Created by FM on 13.05.16.
  */
case class PinToTimePoint(timePoint: TimePoint) {
  def apply(dataStream: Stream): Set[AspRule] = {
    val nowAtT = apply(now)

    val pinnedAtoms = dataStream flatMap (x => PinToTimePoint(x.time).atoms(x.atoms))

    pinnedAtoms + nowAtT
  }

  def atoms(atoms: Set[Atom]): Set[AspRule] = {
    atoms map (apply(_))
  }

  def apply(atom: Atom): AspRule = {
    AspFact(atom(timePoint))
  }

  def apply(program: AspProgram, dataStream: Stream): AspProgramAtTimePoint = {
    AspProgramAtTimePoint(program, apply(dataStream), timePoint)
  }
}

case class AspProgramAtTimePoint(baseProgram: AspProgram, pinnedAtoms: Set[AspRule], timePoint: TimePoint) extends AspProgram {
  val rules: Seq[AspRule] = baseProgram.rules ++ pinnedAtoms
}