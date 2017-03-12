package core.grounding

import core._
import core.lars.Assignment

/**
  * Created by hb on 08.03.17.
  */
object Grounding {

  //note: creating new traits and case classes for 'RelationAtoms' would complicate things too much
  //i) we would have mixes of {GroundAtom,NonGroundAtom} with {RelationAtom, normal atoms}
  //ii) ensure that for the special predicate names, RelationAtom instances are generated
  //there, one still would have to make a pattern matching over predicate names somewhere/somehow
  //since this plays a role only at the level of grounding, we can do it here directly and keeping
  //the class hierarchy of atoms simple/uniform.
  /*
  def isRelationAtom(x: ExtendedAtom): Boolean = x match {
    case a: Atom => a.predicate.caption match {
      case "neq" => true
      case "eq" => true
      case "leq" => true
      case "lt" => true
      case "geq" => true
      case "gt" => true
      case "pow" => true
      case "mod" => true
      case "sum" => true
      case "prod" => true
      case _ => false
    }
    case _ => false
  }
   */

  def holds(relationAtom: RelationAtom): Boolean = {
    if (!relationAtom.isGround())
      throw new RuntimeException("illegal use. relation atom must be ground when tested whether it holds.")

    val atom = relationAtom.asInstanceOf[GroundAtomWithArguments]
    val args = atom.arguments

    def i(idx: Int): Int = args(idx) match {
      case IntValue(k) => k
      case StringValue(s) => Integer.parseInt(s)
      case _ => throw new RuntimeException("unknown value " + args(idx) + " in non-ground relation atom " + atom)
    }

    relationAtom.predicate.caption match {
      case "eq" => args(0) == args(1)
      case "neq" => args(0) != args(1)
      case "leq" => i(0) <= i(1)
      case "lt" => i(0) < i(1)
      case "geq" => i(0) >= i(1)
      case "gt" => i(0) > i(1)
      case "pow" => Math.pow(i(0), i(1)).toInt == i(2)
      case "mod" => (i(1) > 0) && (i(0) % i(1) == i(2))
      case "sum" => i(0) + i(1) == i(2)
      case "prod" => i(0) * i(1) == i(2)
      case _ => false
    }
  }


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
    groundRelationAtoms forall holds
  }

  def assign(relationAtom: RelationAtom, partialBindings: Set[(Variable, Value)]): RelationAtom = {
    val assignment = Assignment(partialBindings.toMap)
    relationAtom.assign(assignment).asInstanceOf[RelationAtom]
  }

}
