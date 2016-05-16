package engine.asp.evaluation

import core.Atom
import core.asp.{AspProgram, AspRule}
import core.lars.TimePoint

/**
  * Created by FM on 16.05.16.
  */
// TODO: naming
object GroundPinnedAsp {
  def apply(program: AspProgramAtTimePoint): GroundedAspProgram = {
    val atoms = program.pinnedAtoms map GroundedAspRule

    GroundedAspProgram(AspProgram(), atoms, 1)
  }
}

case class GroundPinnedAsp(timePoint: TimePoint) {
  def apply(atom: Atom) = {

  }
}

case class GroundedAspRule(rule: PinnedAspRule) extends AspRule {
  override val pos: Set[Atom] = rule.pos.toSet
  override val neg: Set[Atom] = rule.neg.toSet
  override val head: Atom = rule.head
}

case class GroundedAspProgram(baseProgram: AspProgram, groundedAtoms: Set[GroundedAspRule], timePoint: TimePoint) extends AspProgram {
  val rules: Seq[AspRule] = baseProgram.rules ++ groundedAtoms
}
