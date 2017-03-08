package core.grounding

import core._
import core.lars.{Assignment, ExtendedAtom, HeadAtom}
import core.grounding.Grounding._


/*
 * Works for ASP and LARS programs without @
 */
class RuleGrounder[TRule <: Rule[THead, TBody], THead <: HeadAtom, TBody <: ExtendedAtom] {

  def groundWith(inspect: ProgramInspection[TRule, THead, TBody])(rule: TRule): Set[TRule] = {
    if (rule isGround) {
      if (rule.isFact) return Set(rule)
      else return Set(rule) filter relationsHold map deleteAuxiliaryAtoms
    }
    val possibleVariableValues: Map[Variable, Set[Value]] = inspect possibleVariableValues rule
    ground(rule, possibleVariableValues)
  }

  def relationsHold(rule: TRule): Boolean = {
    (rule.pos map (_.atom) filter isRelationAtom forall (holds(_))) &&
    (rule.neg map (_.atom) filter isRelationAtom forall (!holds(_)))
  }

  def deleteAuxiliaryAtoms(rule: TRule): TRule = {
    val corePosAtoms: Set[TBody] = rule.pos filter (!isRelationAtom(_))
    val coreNegAtoms: Set[TBody] = rule.neg filter (!isRelationAtom(_))
    rule.from(rule.head, corePosAtoms, coreNegAtoms).asInstanceOf[TRule]
  }

  def ground(rule: TRule, possibleValuesPerVariable: Map[Variable, Set[Value]]): Set[TRule] = {
    val relationAtoms: Set[AtomWithArgument] = rule.atoms collect { case a: AtomWithArgument if isRelationAtom(a) => a }
    def holdsPartially = allGroundedRelationsHold(relationAtoms) _

    val pairSingletonsPerVariable: Seq[Set[Set[(Variable, Value)]]] = makePairedWithValueSingletons(possibleValuesPerVariable)

    val preparedAssignments: Set[Set[(Variable, Value)]] = {
      pairSingletonsPerVariable.reduce((s1, s2) => cross(s1, s2) filter holdsPartially) //filter
    }

    val groundRules: Set[TRule] = assign(rule, preparedAssignments) //assign
    groundRules map deleteAuxiliaryAtoms //cut
  }

  def assign(rule: TRule, preparedAssignments: Set[Set[(Variable, Value)]]): Set[TRule] = {
    preparedAssignments map (a => rule.assign(Assignment(a.toMap)).asInstanceOf[TRule])
  }

}

