package engine.asp

import core.asp.{AspFact, AspRule, NormalRule}
import core.{Atom, PinnedAtom, Predicate, RelationAtom}
import core.lars._

/**
  * Created by fm on 20/02/2017.
  */
trait LarsToAspMapper {

  def windowAtomEncoder(windowAtom: WindowAtom, groundingGuards: Set[Atom]=Set()): WindowAtomEncoder = windowAtom match {
    case w@WindowAtom(window: SlidingTimeWindow, _, _) => slidingTime(window, w, groundingGuards)
    case w@WindowAtom(window: SlidingTupleWindow, _, _) => slidingTuple(window, w, groundingGuards)
  }

  def encodeRule(rule: LarsRule): LarsRuleEncoding = {
    val encodedRule = encode(rule)

    val groundingGuards: Map[WindowAtom,Set[Atom]] = extractGroundingGuards(rule)

    val windowAtomEncoders = (rule.pos ++ rule.neg) collect {
      case wa: WindowAtom => windowAtomEncoder(wa,groundingGuards.getOrElse(wa,Set()))
    }

    LarsRuleEncoding(rule, encodedRule, windowAtomEncoders)
  }

  def extractGroundingGuards(rule: LarsRule): Map[WindowAtom,Set[Atom]] = {
    val variables = rule.pos collect {
      case wa: WindowAtom => (wa, wa.atom.variables)
    }
    val posAtoms = rule.pos collect { case a:Atom if !a.isInstanceOf[RelationAtom] => a }
    variables map {
      case (wa,wVars) => {
        val guards = posAtoms filter (a => !a.variables.intersect(wVars).isEmpty)
        (wa,guards)
      }
    } toMap
  }

  def predicateFor(window: WindowAtom): Predicate = predicateFor(window.windowFunction, window.temporalModality, window.atom)

  private def predicateFor(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) = {
    val window = windowFunction match {
      case SlidingTimeWindow(size) => f"w_te_${timePoints(size.unit, size.length)}"
      case SlidingTupleWindow(size) => f"w_tu_$size"
      case FluentWindow => f"w_fl"
    }
    val operator = temporalModality match {
      case Diamond => "d"
      case Box => "b"
      case a: At => f"at_${a.time}"
    }
    val atomName = atom match {
      case p: PinnedAtom => p.atom.predicate
      case _ => atom.predicate
    }
    Predicate(f"${window}_${operator}_${atomName.toString}")
  }

  def encode(rule: LarsRule): NormalRule = {
    AspRule(
      encodingAtom(rule.head),
      rule.pos map encodingAtom,
      rule.neg map encodingAtom
    )
  }

  def apply(program: LarsProgram): LarsProgramEncoding = {

    val (backgroundKnowledge,nonFacts) = program.rules partition (_.isFact)

    val backgroundFacts: Seq[NormalRule] = backgroundKnowledge map (r => AspFact[Atom](r.head.asInstanceOf[Atom])) //ignore potential @-atom facts
    val backgroundPredicates = backgroundKnowledge map (_.head.atom.predicate) toSet

    val actualProgram = LarsProgram(nonFacts)

    val nowAndAtNowIdentityRules = actualProgram.atoms.filter(a => !backgroundPredicates.contains(a.predicate)). //assumption on use of background data
      flatMap(identityRulesForAtom).
      toSeq

    val rulesEncodings = actualProgram.rules map encodeRule

    LarsProgramEncoding(rulesEncodings, nowAndAtNowIdentityRules, backgroundFacts)
  }

  def identityRulesForAtom(a: Atom): Seq[NormalRule]

  def encodingAtom(extendedAtom: ExtendedAtom): Atom

  def slidingTime(window: SlidingTimeWindow, windowAtom: WindowAtom, groundingGuards: Set[Atom]): WindowAtomEncoder

  def slidingTuple(window: SlidingTupleWindow, windowAtom: WindowAtom, groundingGuards: Set[Atom]): WindowAtomEncoder

  def timePoints(unit: TimeUnit, size: Long): Long
}
