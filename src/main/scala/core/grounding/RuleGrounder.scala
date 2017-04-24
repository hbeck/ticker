package core.grounding

import core._
import core.grounding.Grounding.{allGroundedRelationsHold, cross, makePairedWithValueSingletons}
import core.lars.{Assignment, ExtendedAtom, HeadAtom}

case class RuleGrounder[TRule <: Rule[THead, TBody], THead <: HeadAtom, TBody <: ExtendedAtom](inspect: ProgramInspection[TRule, THead, TBody]) {

  def ground(rule: TRule, ensureGroundResult: Boolean = true): Set[TRule] = {
    if (rule isGround) {
      if (rule.isFact) return Set(rule)
      else return ensureRelations(Set(rule), ensureGroundResult)
    }
    val possibleVariableValues: Map[Variable, Set[Value]] = inspect.possibleVariableValues(rule,ensureGroundResult)
    ground(rule, possibleVariableValues, ensureGroundResult)
  }

  def ground(rule: TRule, possibleValuesPerVariable: Map[Variable, Set[Value]], ensureGroundResult: Boolean): Set[TRule] = {

    if (possibleValuesPerVariable.isEmpty) {
      if (ensureGroundResult) {
        throw new RuntimeException(f"no possibleValuesPerVariable for rule $rule")
      } else {
        return Set(rule)
      }
    }

    val relationAtoms: Set[RelationAtom] = rule.pos collect { case a: RelationAtom => a }
    //def holdsPartially = allGroundedRelationsHold(relationAtoms) _

    val pairSingletonsPerVariable: Seq[Set[Set[(Variable, Value)]]] = makePairedWithValueSingletons(possibleValuesPerVariable)

    val preparedAssignments: Set[Set[(Variable, Value)]] = {
      if (possibleValuesPerVariable.size == 1) {
        pairSingletonsPerVariable.head filter (allGroundedRelationsHold(relationAtoms, _)) //TODO
      } else {
        //pairSingletonsPerVariable.reduce((s1, s2) => cross(s1, s2) filter holdsPartially) //filter
        pairSingletonsPerVariable.reduce((s1, s2) => cross(s1, s2) filter (allGroundedRelationsHold(relationAtoms, _))) //filter
      }
    }

    val groundRules: Set[TRule] = assign(rule, preparedAssignments) //assign
    groundRules map (deleteAuxiliaryAtoms(_,ensureGroundResult)) //cut
  }

  def assign(rule: TRule, preparedAssignments: Set[Set[(Variable, Value)]]): Set[TRule] = {
    preparedAssignments map (a => rule.assign(Assignment(a.toMap)).asInstanceOf[TRule])
  }

  def ensureRelations(rules: Set[TRule], ensureGroundResult: Boolean): Set[TRule] = {
    rules collect {
      case r if relationsHold(r,ensureGroundResult) => deleteAuxiliaryAtoms(r,ensureGroundResult)
    }
  }

  def ensureRuleRelations(rule: TRule, ensureGroundResult: Boolean): Option[TRule] = {
    if (relationsHold(rule, ensureGroundResult)) {
      Some(deleteAuxiliaryAtoms(rule,ensureGroundResult))
    } else {
      None
    }
  }

  def relationsHold(rule: TRule, ensureGroundResult: Boolean): Boolean = {
    if (ensureGroundResult) {
      rule.pos collect { case ra:RelationAtom => ra } forall (_.groundingHolds)
    } else {
      rule.pos collect { case ra:RelationAtom if ra.isGround => ra} forall (_.groundingHolds)
    }
    //&& (rule.neg collect { case ra:RelationAtom => ra } forall (!_.groundingHolds())) //not used per convention
  }

  def deleteAuxiliaryAtoms(rule: TRule, ensureGroundResult: Boolean): TRule = {
    val posAtomsSansGroundRelations: Set[TBody] = {
      if (ensureGroundResult) {
        rule.pos filterNot (_.isInstanceOf[RelationAtom])
      } else {
        rule.pos filterNot (a => (a.isInstanceOf[RelationAtom] && a.atom.isGround))
      }
    }
    //val coreNegAtoms: Set[TBody] = rule.neg filterNot (_.isInstanceOf[RelationAtom]) //not used per convention
    //rule.from(rule.head, corePosAtoms, coreNegAtoms).asInstanceOf[TRule]
    rule.from(rule.head,posAtomsSansGroundRelations,rule.neg).asInstanceOf[TRule]
  }

}