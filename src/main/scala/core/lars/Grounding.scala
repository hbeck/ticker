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
    val possibleValuesPerVariable: Map[Variable, Set[Value]] = inspect possibleValuesPerVariable rule
    val assignments: Set[Assignment] = createAssignments(possibleValuesPerVariable)
    assignments map (rule.assign(_))
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

/*
case class LarsProgramInspection(program: LarsProgram) {

  //def justifications(a: Atom): Set[LarsRule] = program.rules filter (_.head == a) toSet

  val rules = program.rules.toSet

  //val ruleHeadAtoms = rules map (_.head) collect { case x if x.isInstanceOf[Atom] => x.asInstanceOf[Atom] } //ignore AtAtoms!
  val facts = rules filter (_.isFact)

  val factAtoms = facts map (_.head) collect { case x if x.isInstanceOf[Atom] => x.asInstanceOf[Atom] }
  //val intensionalAtoms = ruleHeadAtoms diff factAtoms
//  val signals = rules flatMap (r => r.pos union r.neg) collect {
//    case x if x.isInstanceOf[WindowAtom] => x.asInstanceOf[WindowAtom].atom
//  }

  //val allExtendedAtoms: Set[ExtendedAtom] = (rules flatMap (_.atoms)) toSet

  val groundFactAtoms = factAtoms filter (_.isGround)
  val unaryGroundFactAtoms: Map[String, Set[Atom]] = groundFactAtoms filter (_.arity == 1) groupBy (_.predicateSymbol())
  val nonUnaryGroundFactAtoms: Map[String, Set[Atom]] = groundFactAtoms filter (_.arity > 1) groupBy (_.predicateSymbol())

  //("a" -> {a(x), a(y)})  ==>  ("a" -> {x,y})
  val unaryLookup: Map[String,Set[Value]] = unaryGroundFactAtoms mapValues { set =>
    set map (atom => atom.asInstanceOf[AtomWithArgument].arguments.head.asInstanceOf[Value])
  }

  //("a" -> {a(x,y), a(z,w)})  ==>  ("a" -> (0 -> {x,z}, 1 -> {y,w}))
  val nonUnaryLookup = nonUnaryGroundFactAtoms mapValues { set =>
    set.map(_.asInstanceOf[AtomWithArgument].arguments) //{a(x,y), a(z,y)} ==> {(x,z), (y,w)}
      .flatMap(_.zipWithIndex) // ==> {(0,x), (1,y), (0,z), (1,w)}
      .groupBy (_._2) //Map(1 -> {(y,1), (w,1)}, 0 -> {(x,0), (z,0)})
      .mapValues ( _ map (pair => pair._1) ) //Map(1 -> {y,w}, 0 -> {x,z})
  }

  //rule parts with variables
  val intensionalVariableOccurrences: Map[LarsRule, Map[Variable,Set[Atom]]]
  val unaryFactAtomVariableOccurrences: Map[LarsRule,Map[Variable,Set[Atom]]] //extensional unary atoms in rule for which ground fact exists per assumption
  val nonUnaryFactAtomVariableOccurrences: Map[LarsRule,Map[Variable,Set[Atom]]] //extensional atoms for which non-unary ground fact exists per assumption

  def possibleValuesPerVariable(rule: LarsRule): Map[Variable, Set[Value]] = {
    rule.variables() map (v => (v,possibleValuesForVariable(rule,v))) toMap
  }

  def possibleValuesForVariable(rule: LarsRule, variable: Variable): Set[Value] = {

    val unaryFactAtoms = unaryFactAtomVariableOccurrences(rule).getOrElse(variable,Set())
    //pick random, better use the one with minimal number of values
    if (!unaryFactAtoms.isEmpty) {
      return unaryLookup(unaryFactAtoms.head.predicateSymbol())
    }

    val nonUnaryFactAtoms = nonUnaryFactAtomVariableOccurrences(rule).getOrElse(variable,Set())
    //pick random, better use the one with minimal number of values
    if (!nonUnaryFactAtoms.isEmpty) {

    }


    //per convention, the variable now cannot occur in a signal
    //only the case remains that the variable appears only in intensional atoms




    Set()
  }

  def isUnaryFact(rule: LarsRule): Boolean = {
    if (!rule.isFact) return false
    rule.head match {
      case AtAtom(time,atom) => atom.arity == 1
      case a:Atom => a.arity == 1
    }
  }


}
*/