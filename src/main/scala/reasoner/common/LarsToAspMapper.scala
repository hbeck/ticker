package reasoner.common

import core.asp.{AspFact, AspRule, NormalRule}
import core.lars._
import core.{Atom, PinnedAtom, Predicate, RelationAtom}


/**
  * Created by fm, hb 02/2017.
  */
trait LarsToAspMapper {

  def windowAtomEncoder(windowAtom: WindowAtom, groundingGuards: Set[Atom]=Set()): WindowAtomEncoder = windowAtom match {
    case w@WindowAtom(window: TimeWindow, _, _) => slidingTime(window, w, groundingGuards)
    case w@WindowAtom(window: TupleWindow, _, _) => slidingTuple(window, w, groundingGuards)
  }

  def encodeRule(rule: LarsRule): LarsRuleEncoding = {
    val baseRule = encode(rule)

    val groundingGuards: Map[WindowAtom,Set[Atom]] = LarsToAspMapper.extractGroundingGuards(rule)

    val posWindowAtomEncoders = rule.pos collect {
      case wa: WindowAtom => windowAtomEncoder(wa,groundingGuards.getOrElse(wa,Set()))
    }

    //separation of pos and neg to determine duration of base rule
    val negWindowAtomEncoders = rule.neg collect {
      case wa: WindowAtom => windowAtomEncoder(wa,groundingGuards.getOrElse(wa,Set()))
    }

    LarsRuleEncoding(rule, baseRule, posWindowAtomEncoders, negWindowAtomEncoders)
  }

  def predicateFor(window: WindowAtom): Predicate = predicateFor(window.windowFunction, window.temporalModality, window.atom)

  private def predicateFor(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) = {
    val window = windowFunction match {
      case TimeWindow(size) => f"w_te_${timePoints(size.unit, size.length)}"
      case TupleWindow(size) => f"w_tu_$size"
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

    val actualProgram = LarsProgram(nonFacts)

    val scoped = (program.windowAtoms map (_.atom.predicate)) union (program.atAtoms map (_.atom.predicate))
    val headPredicates = (program.rules map (_.head.atom.predicate)).toSet
    val nonScopedIntensional = headPredicates diff scoped
    val nonScopedExtensional = program.atoms map (_.predicate) diff headPredicates diff scoped
    val nonQ = nonScopedIntensional union nonScopedExtensional union (backgroundKnowledge map (_.head.atom.predicate) toSet)
    val Q = (actualProgram.atoms map (_.predicate)) diff nonQ
    val needGuard = scoped diff headPredicates

    val nowAndAtNowIdentityRules = actualProgram.atoms.filter(a => Q.contains(a.predicate)). //assumption on use of background data
      flatMap(identityRulesForAtom).
      toSeq

    val rulesEncodings = actualProgram.rules map encodeRule

    LarsProgramEncoding(rulesEncodings, nowAndAtNowIdentityRules, backgroundFacts, needGuard)
  }

  def identityRulesForAtom(a: Atom): Seq[NormalRule]

  def encodingAtom(extendedAtom: ExtendedAtom): Atom

  def slidingTime(window: TimeWindow, windowAtom: WindowAtom, groundingGuards: Set[Atom]): WindowAtomEncoder

  def slidingTuple(window: TupleWindow, windowAtom: WindowAtom, groundingGuards: Set[Atom]): WindowAtomEncoder

  def timePoints(unit: TimeUnit, size: Long): Long
}

object LarsToAspMapper {
  def extractGroundingGuards(rule: LarsRule): Map[WindowAtom,Set[Atom]] = {
    val windowAtoms = rule.body collect { case wa:WindowAtom => wa }
    windowAtoms map (wa => (wa,extractGroundingGuards(rule,wa))) toMap
//    val windowAtomWithVariables = rule.pos collect {
//      case wa: WindowAtom => (wa, wa.atom.variables)
//    }
//    val posAtoms = rule.pos collect { case a:Atom if !a.isInstanceOf[RelationAtom] => a }
//    windowAtomWithVariables map {
//      case (wa,wVars) => {
//        val guards = posAtoms filter (a => !a.variables.intersect(wVars).isEmpty)
//        (wa,guards)
//      }
//    } toMap
  }

  def extractGroundingGuards(rule: LarsRule, extendedAtom: ExtendedAtom): Set[Atom] = {
    val posAtoms = rule.pos collect { case a:Atom if !a.isInstanceOf[RelationAtom] => a }
    posAtoms filter (a => !a.variables.intersect(extendedAtom.atom.variables).isEmpty)
  }

  def findGroundingGuards(larsProgramEncoding: LarsProgramEncoding, atom: Atom): Set[Atom] = {
    val optRule = larsProgramEncoding.larsRules find { rule =>
      rule.body exists {
        case wa: WindowAtom if wa.atom == atom => true
        //case aa: AtAtom if aa.atom == atom => true //ignored
        case _ => false
      }
    }

    optRule match {
      case None =>
        throw new RuntimeException(f"no guard found for atom "+atom)
      case Some(rule) => extractGroundingGuards(rule,atom) //assumption that guard is given everywhere, i.e., can choose any
    }

  }
}