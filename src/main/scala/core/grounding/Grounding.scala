package core.grounding

import core._
import core.lars.Assignment

/**
  * Created by hb on 08.03.17.
  */
object Grounding {

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

  def allGroundedRelationsHold(relationAtoms: Set[RelationAtom])(partialAssignment: Set[(Variable, Value)]): Boolean = {
    val groundRelationAtoms: Set[RelationAtom] = relationAtoms map (assign(_, partialAssignment)) filter (_.isGround)
    groundRelationAtoms forall (_.groundingHolds())
  }

  def assign(relationAtom: RelationAtom, partialBindings: Set[(Variable, Value)]): RelationAtom = {
    val assignment = Assignment(partialBindings.toMap)
    relationAtom.assign(assignment).asInstanceOf[RelationAtom]
  }

}
