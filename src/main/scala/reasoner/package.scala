import core.asp.{AspFact, AspProgram, AspRule, NormalRule}
import core._
import core.lars.{LarsRule, TimePoint, TimeVariableWithOffset}
import reasoner.common.{DefaultTrackedSignal, Tick}

/**
  * Created by FM on 05.04.16.
  */
package object reasoner {

  // From an outside perspective we only want to pass in anonymous data
  //case class EngineAtom(name: String, arguments: Seq[String] = List())

  //single 'line' in experimental.evaluation function
  case class StreamEntry(time: TimePoint, atoms: Set[Atom])

  type Stream = Set[StreamEntry]

  type SignalStream = Set[DefaultTrackedSignal]

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


  //keep original lars rule from which a pinned rule stems
  type LarsRuleAsPinnedRules = (LarsRule, Set[PinnedRule])
  type LarsRuleAsAspRules = (LarsRule, Set[NormalRule])

  type GroundAspRule = AspRule[GroundAtom]
  type GroundAspFact = AspFact[GroundAtom]
  type GroundedAspStream = Set[GroundAspFact]
  type GroundAspProgram = AspProgram[GroundAtom, GroundAspRule]

}



