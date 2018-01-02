package reasoner

import core.asp.{AspFact, AspProgram, AspRule, NormalRule}
import core.lars._
import core._
import reasoner.common.Tick

/**
  * Created by FM on 13.05.16.
  */
package object asp {

  val now = Predicate("now")
  val cnt = Predicate("cnt")
  val tickPredicate = Predicate("tick")

  val TimePinVariableName = "NN"
  val CountPinVariableName = "CC"
  val TimePinVariable = TimeVariableWithOffset(Variable(TimePinVariableName))
  val CountPinVariable = Variable(CountPinVariableName)

  def tickAtom(time: Argument, count: Argument): AtomWithArguments = AtomWithArguments(tickPredicate,Seq(time,count))
  def tickFact(time: Argument, count: Argument): AspFact[AtomWithArguments] = AspFact(tickAtom(time,count))
  def tickFactAsNormalRule(time: Argument, count: Argument): NormalRule = AspFact(tickAtom(time,count))

  type TickDuration = Tick //subsumes previous TicksUntilExpired, TicksUntilOutdated
  val Void: Long = -1L //states irrelevance of tick dimension

  val specialPinPredicates = Seq(now, cnt) //note that "tick" is not used for pinning!

  type PinnedRule = AspRule[AtomWithArguments]
  type PinnedFact = AspFact[AtomWithArguments]
  type PinnedProgram = AspProgram[AtomWithArguments, PinnedRule]

  type PinnedModel = Set[Atom]
  type PinnedStream = Set[PinnedFact]

  //keep original lars rule from which a pinned rule stems
  type LarsRuleAsPinnedRules = (LarsRule, Set[PinnedRule])
  type LarsRuleAsAspRules = (LarsRule, Set[NormalRule])

  type GroundAspRule = AspRule[GroundAtom]
  type GroundAspFact = AspFact[GroundAtom]
  type GroundedAspStream = Set[GroundAspFact]
  type GroundAspProgram = AspProgram[GroundAtom, GroundAspRule]
}
