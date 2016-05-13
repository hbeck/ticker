package engine.asp

import core.Atom
import core.asp.{AspFact, AspProgram, AspRule}
import core.lars.TimePoint
import engine._

/**
  * Created by FM on 13.05.16.
  */
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
    PinnedAspRule(AspFact(atom(timePoint)))
  }

  def apply(program: AspProgram, dataStream: Stream): AspProgramAtTimePoint = {
    AspProgramAtTimePoint(program, apply(dataStream), timePoint)
  }
}

case class PinnedAspRule(rule: AspRule) extends AspRule {
  override val pos: Set[Atom] = rule.pos
  override val neg: Set[Atom] = rule.neg
  override val head: Atom = rule.head
}

case class AspProgramAtTimePoint(baseProgram: AspProgram, pinnedAtoms: Set[PinnedAspRule], timePoint: TimePoint) extends AspProgram {
  val rules: Seq[AspRule] = baseProgram.rules ++ pinnedAtoms
}