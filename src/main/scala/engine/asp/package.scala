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
  val timePredicate = Predicate("time")

  def tickAtom(time: Argument, count: Argument): AtomWithArguments = AtomWithArguments(tickPredicate,Seq(time,count))
  def tickFact(time: Argument, count: Argument): AspFact[AtomWithArguments] = AspFact(tickAtom(time,count))
  def tickFactAsNormalRule(time: Argument, count: Argument): NormalRule = AspFact(tickAtom(time,count))
  //def timeAtom(time: Argument): AtomWithArguments = AtomWithArguments(timePredicate,Seq(time))
  //def timeFact(time: Argument): AspFact[AtomWithArguments] = AspFact(timeAtom(time))
  //def timeFactAsNormalRule(time: Argument): NormalRule = AspFact(timeAtom(time))

  //naming: *expiration* is a tick when a rule *must* be removed, whereas an *outdated* rule *can* be removed
  //use -1 for infinity
  @deprecated type Expiration = Tick //time, count; now using simply Tick
  @deprecated type Outdate = Tick //time, count; now using simply Tick
  type TickDuration = Tick //subsumes previous TicksUntilExpired, TicksUntilOutdated
  @deprecated type TicksUntilExpiration = Tick
  @deprecated type TicksUntilOutdated = Tick
  val Void: Long = -1L


  val specialPinPredicates = Seq(now, cnt) //note that "tick" is not used for pinning!

  type PinnedRule = AspRule[AtomWithArguments]
  type PinnedFact = AspFact[AtomWithArguments]
  type PinnedProgram = AspProgram[AtomWithArguments, PinnedRule]

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
