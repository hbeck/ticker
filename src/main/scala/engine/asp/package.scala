package engine

import core.asp.{AspFact, AspProgram, AspRule, NormalRule}
import core.lars._
import core._

/**
  * Created by FM on 13.05.16.
  */
package object asp {
  val now = Atom("now")
  val cnt = Atom("cnt")

  type PinnedRule = AspRule[AtomWithArgument]
  type PinnedFact = AspFact[AtomWithArgument]
  type PinnedProgram = AspProgram[AtomWithArgument, PinnedRule]

  type PinnedModel = Set[PinnedAtom]
  type PinnedStream = Set[PinnedFact]

  //keep original lars rule from which a pinned rule stems
  type LarsRuleAsPinnedRules = (LarsRule, Set[PinnedRule])
  type LarsRuleAsAspRules = (LarsRule, Set[NormalRule])

  type GroundAspRule = AspRule[GroundAtom]
  type GroundAspFact = AspFact[GroundAtom]
  type GroundedAspStream = Set[GroundAspFact]
  type GroundAspProgram = AspProgram[GroundAtom, GroundAspRule]
}
