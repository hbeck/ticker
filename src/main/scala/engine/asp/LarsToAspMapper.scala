package engine.asp

import core.asp.{AspRule, NormalRule}
import core.{Atom, PinnedAtom, Predicate}
import core.lars._

/**
  * Created by fm on 20/02/2017.
  */
trait LarsToAspMapper {

  def windowAtomEncoder(windowAtom: WindowAtom): WindowAtomEncoder = windowAtom match {
    case w@WindowAtom(window: SlidingTimeWindow, _, _) => slidingTime(window, w)
    case w@WindowAtom(window: SlidingTupleWindow, _, _) => slidingTuple(window, w)
  }

  def encodeRule(rule: LarsRule): LarsRuleEncoding = {
    val encodedRule = encode(rule)

    val windowAtomEncoders = (rule.pos ++ rule.neg) collect {
      case wa: WindowAtom => windowAtomEncoder(wa)
    }

    LarsRuleEncoding(rule, encodedRule, windowAtomEncoders)
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

    val backgroundData = backgroundKnowledge map (_.head.asInstanceOf[Atom]) toSet //ignore potential @-atom facts

    val actualProgram = LarsProgram(nonFacts)

    val nowAndAtNowIdentityRules = actualProgram.atoms.diff(backgroundData). //assumption on use of background data
      flatMap(identityRulesForAtom).
      toSeq

    val rulesEncodings = actualProgram.rules map encodeRule

    LarsProgramEncoding(rulesEncodings, nowAndAtNowIdentityRules, backgroundData)
  }

  def identityRulesForAtom(a: Atom): Seq[NormalRule]

  def encodingAtom(extendedAtom: ExtendedAtom): Atom

  def slidingTime(window: SlidingTimeWindow, windowAtom: WindowAtom): WindowAtomEncoder

  def slidingTuple(window: SlidingTupleWindow, windowAtom: WindowAtom): WindowAtomEncoder

  def timePoints(unit: TimeUnit, size: Long): Long
}
