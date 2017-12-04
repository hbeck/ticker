package core.grounding

import core.asp.NormalRule
import core.lars.Assignment
import core.{RelationAtom, Value, Variable}

import core.Atom

/**
  * Created by hb on 08.03.17.
  */
object Grounding {

  def ensureRuleRelations(rule: NormalRule): Option[NormalRule] = {
    if (relationsHold(rule)) {
      Some(deleteAuxiliaryAtoms(rule))
    } else {
      None
    }
  }

  def relationsHold(rule: NormalRule): Boolean = {
    rule.pos collect { case ra:RelationAtom => ra } forall (_.groundingHolds())
    //&& (rule.neg collect { case ra:RelationAtom => ra } forall (!_.groundingHolds())) //not used per convention
  }

  def deleteAuxiliaryAtoms(rule: NormalRule): NormalRule = {
    val corePosAtoms: Set[Atom] = rule.pos filterNot (_.isInstanceOf[RelationAtom])
    //val coreNegAtoms: Set[TBody] = rule.neg filterNot (_.isInstanceOf[RelationAtom]) //not used per convention
    //rule.from(rule.head, corePosAtoms, coreNegAtoms).asInstanceOf[TRule]
    rule.from(rule.head,corePosAtoms,rule.neg).asInstanceOf[NormalRule]
  }

  //

  // X -> { x1, x2 }
  // Y -> { y1, y2 }
  // Z -> { z1, z2 }
  // =>
  // [ { {(X,x1)}, {(X,x2)} }
  //   { {(Y,y1)}, {(Y,y2)} }
  //   { {(Z,z1)}, {(Z,z2)} } ]
  def makePairedWithValueSingletons(possibleValuesPerVariable: Map[Variable, Set[Value]]): Seq[Set[Set[(Variable, Value)]]] = {
    possibleValuesPerVariable map {
      case (variable, valueSet) => valueSet map (value => Set((variable, value))) // {{(X,x1)},{(X,x2)}}
    } toSeq
  }

  //{ {(X,x1)}, {(X,x2)} } cross { {(Y,y1)}, {(Y,y2)} } cross { {(Z,z1)}, {(Z,z2)} }
  //==>
  //{ {(X,x1),(Y,y1)}, {(X,x2),(Y,y1)}, {(X,x1),(Y,y2)}, {(X,x2),(Y,y2)} } cross { {(Z,z1)}, {(Z,z2)} }
  //==>
  //{ {(X,x1),(Y,y1),(Z,z1)}, {(X,x2),(Y,y1),(Z,z1)}, {(X,x1),(Y,y2),(Z,z1)}, {(X,x2,(Y,y2),(Z,z1)},
  //  {(X,x1),(Y,y1),(Z,z2)}, {(X,x2),(Y,y1),(Z,z2)}, {(X,x1),(Y,y2),(Z,z2)}, {(X,x2,(Y,y2),(Z,z2))} }
  def cross[T](sets1: Set[Set[T]], sets2: Set[Set[T]]): Set[Set[T]] = {
    for (s1 <- sets1; s2 <- sets2) yield s1 union s2
  }

  def allGroundedRelationsHold(relationAtoms: Set[RelationAtom], partialAssignment: Set[(Variable, Value)]): Boolean = {
    val groundRelationAtoms: Set[RelationAtom] = relationAtoms map (assignRelationAtom(_, partialAssignment)) filter (_.isGround)
    groundRelationAtoms forall (_.groundingHolds())
  }

  def assignRelationAtom(relationAtom: RelationAtom, partialBindings: Set[(Variable, Value)]): RelationAtom = {
    val assignment = Assignment(partialBindings.toMap)
    val assign: Atom = relationAtom.assign(assignment)
    assign.asInstanceOf[RelationAtom]
  }

}
