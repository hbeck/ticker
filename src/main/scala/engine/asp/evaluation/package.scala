package engine.asp


import core.PinnedAtom
import core.asp.{AspFact, AspRule, FixedAspProgram}

/**
  * Created by FM on 20.05.16.
  */
package object evaluation {
  type PinnedFact = AspFact[PinnedAtom]
  type PinnedRule = AspRule[PinnedAtom]
  type PinnedProgram = FixedAspProgram[PinnedAtom, PinnedRule]

  type PinnedStream = Set[PinnedFact]

  type GroundedStream = Set[GroundedNormalRule]

  type PinnedModel = Set[PinnedAtom]
}
