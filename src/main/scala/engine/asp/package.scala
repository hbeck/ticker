package engine

import core.asp.{AspFact, AspProgram, AspRule, NormalRule}
import core.lars._
import core._

/**
  * Created by FM on 13.05.16.
  */
package object asp {
  val now = Atom("now") //used in a_at(\vec{X},T)
  val cnt = Atom("cnt") //used in a_cnt(\vec{X},C)
  val pin = Atom("pin") //used in a_at_cnt(\vec{X},T,C) //pin = time + tick

  //naming: *expiration* is a tick when a rule *must* be removed, whereas an *outdated* rule *can* be removed
  //use -1 for infinity
  type Expiration = TickPair //time, count
  //type Outdate = TickPair //time, count
  type TicksUntilExpiration = TickPair
  type TicksUntilOutdated = TickPair
  type Tick = TickPair
  val Void: Long = -1L

  val specialTickAtoms = Seq(now, cnt, pin)
  val specialTickPredicates = specialTickAtoms.map(_.predicate)

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
