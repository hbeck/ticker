package engine.asp


import core.PinnedAtom
import core.asp.{AspFact, AspProgram, AspRule}
import core.lars.LarsRule

/**
  * Created by FM on 20.05.16.
  */
package object evaluation {

  type PinnedFact = AspFact[PinnedAtom]
  type PinnedRule = AspRule[PinnedAtom]
  type PinnedProgram = AspProgram[PinnedAtom, PinnedRule]
  type PinnedModel = Set[PinnedAtom]
  type PinnedStream = Set[PinnedFact]

  type LarsRuleMapping = (LarsRule, Set[PinnedRule])

  type GroundedStream = Set[GroundedNormalFact]


}
