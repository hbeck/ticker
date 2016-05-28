package engine.asp


import core.PinnedAtom
import core.asp.{AspFact, AspProgram, AspRule, FixedAspProgram}
import core.lars.{Program, Rule}

/**
  * Created by FM on 20.05.16.
  */
package object evaluation {

  type PinnedFact = AspFact[PinnedAtom]
  type PinnedRule = AspRule[PinnedAtom]
  type PinnedProgram = AspProgram[PinnedAtom, PinnedRule]
  type PinnedModel = Set[PinnedAtom]
  type PinnedStream = Set[PinnedFact]

  type MappedRule = (Rule, Set[PinnedRule])

  type GroundedStream = Set[GroundedNormalRule]


}
