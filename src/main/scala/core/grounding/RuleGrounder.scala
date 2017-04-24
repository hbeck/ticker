package core.grounding

import core._
import core.grounding.Grounding.{allGroundedRelationsHold, cross, makePairedWithValueSingletons}
import core.lars.{Assignment, ExtendedAtom, HeadAtom}

case class RuleGrounder[TRule <: Rule[THead, TBody], THead <: HeadAtom, TBody <: ExtendedAtom](inspect: ProgramInspection[TRule, THead, TBody]) {

  def ground(rule: TRule, ensureGroundResult: Boolean = true): Set[TRule] = {
    if (rule isGround) {
      if (rule.isFact) return Set(rule)
      else return ensureRelations(Set(rule))
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


  def ensureRelations(rules: Set[TRule]): Set[TRule] = {
    rules collect {
      case r if relationsHold(r) => deleteAuxiliaryAtoms(r)
    }
  }

  def ensureRuleRelations(rule: TRule): Option[TRule] = {
    if (relationsHold(rule)) {
      Some(deleteAuxiliaryAtoms(rule))
    } else {
      None
    }
  }

  def relationsHold(rule: TRule): Boolean = {
    rule.pos collect { case ra:RelationAtom => ra } forall (_.groundingHolds())
    //&& (rule.neg collect { case ra:RelationAtom => ra } forall (!_.groundingHolds())) //not used per convention
  }

  def deleteAuxiliaryAtoms(rule: TRule): TRule = {
    val corePosAtoms: Set[TBody] = rule.pos filterNot (_.isInstanceOf[RelationAtom])
    //val coreNegAtoms: Set[TBody] = rule.neg filterNot (_.isInstanceOf[RelationAtom]) //not used per convention
    //rule.from(rule.head, corePosAtoms, coreNegAtoms).asInstanceOf[TRule]
    rule.from(rule.head,corePosAtoms,rule.neg).asInstanceOf[TRule]
  }

}