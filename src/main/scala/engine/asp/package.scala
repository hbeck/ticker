package engine

import core.asp.{AspFact, AspProgram, AspRule, NormalRule}
import core.lars._
import core._

/**
  * Created by FM on 13.05.16.
  */
package object asp {

  val now = Predicate("now")
  val cnt = Predicate("cnt")
  val tickPredicate = Predicate("tick")

  //naming: *expiration* is a tick when a rule *must* be removed, whereas an *outdated* rule *can* be removed
  //use -1 for infinity
  type Expiration = TickPair //time, count
  //type Outdate = TickPair //time, count
  type TicksUntilExpiration = TickPair
  type TicksUntilOutdated = TickPair
  type Tick = TickPair
  val Void: Long = -1L

  val specialPinPredicates = Seq(now, cnt) //note that "tick" is not used for pinning!

  type PinnedRule = AspRule[AtomWithArgument]
  type PinnedFact = AspFact[AtomWithArgument]
  type PinnedProgram = AspProgram[AtomWithArgument, PinnedRule]

  type PinnedModel = Set[Atom] //TODO AspModel?
  type PinnedStream = Set[PinnedFact]

  //keep original lars rule from which a pinned rule stems
  type LarsRuleAsPinnedRules = (LarsRule, Set[PinnedRule])
  type LarsRuleAsAspRules = (LarsRule, Set[NormalRule])

  type GroundAspRule = AspRule[GroundAtom]
  type GroundAspFact = AspFact[GroundAtom]
  type GroundedAspStream = Set[GroundAspFact]
  type GroundAspProgram = AspProgram[GroundAtom, GroundAspRule]
}
