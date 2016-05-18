package engine.asp.evaluation

import core.{Atom, AtomWithTime}
import core.asp._
import core.lars.{Time, TimePoint}
import engine._
import engine.asp._

/**
  * Created by FM on 13.05.16.
  */
// TODO discuss naming/usage of 'Pin'?
case class PinToTimePoint(timePoint: TimePoint) {
  def apply(dataStream: Stream): Set[PinnedAspRule] = {
    val nowAtT = apply(now)

    val pinnedAtoms = dataStream flatMap (x => PinToTimePoint(x.time).atoms(x.atoms))

    pinnedAtoms + nowAtT
  }

  def atoms(atoms: Set[Atom]): Set[PinnedAspRule] = {
    atoms map (apply(_))
  }

  def apply(atom: Atom): PinnedAspRule = {
    PinnedAspRule(atom(timePoint), Set(), Set())
  }

//  def apply(program: Seq[PinnedAspRule], dataStream: Stream): AspProgramAtTimePoint = {
//    AspProgramAtTimePoint(program, apply(dataStream), timePoint)
//  }
}

// TODO naming?
case class PinnedAspRule(head: AtomWithTime, pos: Set[AtomWithTime] = Set(), neg: Set[AtomWithTime] = Set()) extends AspRuleT[AtomWithTime]

case class PinnedAspProgram(rules: Seq[PinnedAspRule]) extends AspProgramT[AtomWithTime, PinnedAspRule]

// TODO naming?
//case class AspProgramAtTimePoint(baseProgram: Seq[PinnedAspRule], pinnedAtoms: Set[PinnedAspRule], time: Time) extends AspProgramT[AtomWithTime, PinnedAspRule] {
//  val rules: Seq[PinnedAspRule] = baseProgram ++ pinnedAtoms.toSeq
//}