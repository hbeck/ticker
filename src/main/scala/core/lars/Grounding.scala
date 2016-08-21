package core.lars

import core.{Value, Variable}

/**
  * Created by hb on 8/21/16.
  */
object Grounding {

  /*

  def apply(program: LarsProgram): LarsProgram = {
    val inspect = LarsProgramInspection(program)
    val groundRules = program.rules flatMap ground(inspect)
    LarsProgram(groundRules)
  }

  def ground(inspect: LarsProgramInspection)(rule: LarsRule): Set[LarsRule] = {
    if (rule isGround) return Set(rule)
    val possibleValuesPerVariable: Map[Variable, Set[Value]] = inspect possibleValuesForVariable rule
    val assignments: Set[Assignment] = createAssignments(possibleValuesPerVariable)
    var groundRules = Set[LarsRule]()
    for (assignment <- assignments) {
      groundRules = groundRules + rule.assign(assignment)
    }
    groundRules
  }

  */

  def ground[T <: ExtendedAtom](x: T, assignment: Assignment): T = {
    if (x.isGround) return x
    x.assign(assignment).asInstanceOf[T]
  }

  def createAssignments(possibleValuesPerVariable: Map[Variable,Set[Value]]): Set[Assignment] = {

    // X -> { x1, x2 }
    // Y -> { y1, y2 }
    // Z -> { z1, z2 }
    // =>
    // X -> { {(X,x1)}, {(X,x2)} }
    // Y -> { {(Y,y1)}, {(Y,y2)} }
    // Z -> { {(Z,z1)}, {(Z,z2)} }
    val pairSingletons: Map[Variable,Set[Set[(Variable,Value)]]] = makePairedWithValueSingletons(possibleValuesPerVariable)

    val seq: Seq[Set[Set[(Variable,Value)]]] = pairSingletons.values.toSeq

    //{ {(X,x1)}, {(X,x2)} } cross { {(Y,y1)}, {(Y,y2)} } cross { {(Z,z1)}, {(Z,z2)} }
    //==>
    //{ {(X,x1),(Y,y1)}, {(X,x2),(Y,y1)}, {(X,x1),(Y,y2)}, {(X,x2),(Y,y2)} } cross { {(Z,z1)}, {(Z,z2)} }
    //==>
    //{ {(X,x1),(Y,y1),(Z,z1)}, {(X,x2),(Y,y1),(Z,z1)}, {(X,x1),(Y,y2),(Z,z1)}, {(X,x2,(Y,y2),(Z,z1)},
    //  {(X,x1),(Y,y1),(Z,z2)}, {(X,x2),(Y,y1),(Z,z2)}, {(X,x1),(Y,y2),(Z,z2)}, {(X,x2,(Y,y2),(Z,z2))} }
    val preparedAssignments: Set[Set[(Variable, Value)]] = seq.reduce((s1, s2) => cross(s1,s2))

    preparedAssignments map (set => Assignment(set.toMap))
  }

  def makePairedWithValueSingletons(possibleValuesPerVariable: Map[Variable,Set[Value]]): Map[Variable,Set[Set[(Variable,Value)]]] = {
    possibleValuesPerVariable map { x =>
      val variable = x._1
      val valueSet = x._2
      val pairedWithValue: Set[(Variable,Value)] = valueSet map (value => (variable,value))
      (variable,Set(pairedWithValue))
    }
  }

  def cross[T](set1: Set[Set[T]], set2: Set[Set[T]]): Set[Set[T]] = {
    for (x <- set1; y <- set2) yield x union y
  }

}

case class LarsProgramInspection(program: LarsProgram) {
  //def possibleValuesForVariable(rule: LarsRule): Map[Variable, Set[Value]] = ???
}