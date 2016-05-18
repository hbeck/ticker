package engine.asp.evaluation

import core.{Atom, AtomWithTime}
import core.asp.{AspProgram, AspRule, AspRuleT}
import core.lars.{TimePoint, TimeVariable}

/**
  * Created by FM on 16.05.16.
  */
// TODO: naming
case class GroundPinnedAsp(timePoint: TimePoint) {
  def apply(atom: AtomWithTime) = {
    // TODO: move into AtomWithTime.ground Function?
    val groundedTimePoint = atom.time match {
      case v: TimeVariable => v.ground(timePoint)
      case t: TimePoint => t
    }
    atom.atom(groundedTimePoint)
  }

  def apply(program: PinnedAspProgram, dataStream: Set[PinnedAspRule]): GroundedAspProgram = {
    val atoms = dataStream map apply

    GroundedAspProgram(program.rules map apply, atoms, timePoint)
  }

  def apply(pinnedAspRule: PinnedAspRule): GroundedAspRule = {
    GroundedAspRule(
      this.apply(pinnedAspRule.head),
      pinnedAspRule.pos map this.apply,
      pinnedAspRule.neg map this.apply
    )
  }
}

// TODO discuss signature/naming
case class GroundedAspRule(head: Atom, pos: Set[Atom] = Set(), neg: Set[Atom] = Set()) extends AspRuleT[Atom]

// TODO discuss signature/naming
case class GroundedAspProgram(programRules: Seq[GroundedAspRule], groundedAtoms: Set[GroundedAspRule], timePoint: TimePoint) extends AspProgram {
  val rules: Seq[AspRule] = programRules ++ groundedAtoms
}
