package engine.asp


import core.{Fact, GroundAtom, PinnedAtom}
import core.asp.{AspFact, AspProgram, AspRule}
import core.lars.{LarsRule, UserDefinedLarsRule}

/**
  * Created by FM on 20.05.16.
  */
package object evaluation {
  type PinnedRule = AspRule[PinnedAtom]
  type PinnedFact = AspFact[PinnedAtom]
  type PinnedProgram = AspProgram[PinnedAtom, PinnedRule]

  type PinnedModel = Set[PinnedAtom]
  type PinnedStream = Set[PinnedFact]

  type LarsRuleMapping = (LarsRule, Set[PinnedRule])

  type GroundRule = AspRule[GroundAtom]
  type GroundFact = AspFact[GroundAtom]
  type GroundedStream = Set[GroundFact]
  type GroundProgram = AspProgram[GroundAtom, GroundRule]


}
