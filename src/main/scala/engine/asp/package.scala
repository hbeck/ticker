package engine

import core.asp.{AspFact, AspProgram, AspRule}
import core.lars._
import core._

/**
  * Created by FM on 13.05.16.
  */
package object asp {
  val now = Atom("now")

  type PinnedRule = AspRule[AtomWithArgument]
  type PinnedFact = AspFact[AtomWithArgument]
  type PinnedProgram = AspProgram[AtomWithArgument, PinnedRule]

  type PinnedModel = Set[PinnedAtom]
  type PinnedStream = Set[PinnedFact]

  type LarsRuleMapping = (LarsRule, Set[PinnedRule])

  type GroundRule = AspRule[GroundAtom]
  type GroundFact = AspFact[GroundAtom]
  type GroundedStream = Set[GroundFact]
  type GroundProgram = AspProgram[GroundAtom, GroundRule]
}
