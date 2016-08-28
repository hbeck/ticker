package core.lars

import common.Util.printTime
import core._

import scala.collection.immutable.HashMap


/**
  * applicable only to lars programs without "@"
  *
  * Created by hb on 8/21/16.
  */
case class Grounder(program: LarsProgram) {

  val inspect = LarsProgramInspection(program)
  val groundRules = program.rules flatMap Grounder.ground(inspect)
  val groundProgram = LarsProgram(groundRules)

}

object Grounder {

  def ground(inspect: LarsProgramInspection)(rule: LarsRule): Set[LarsRule] = {
    if (rule isGround) {
      if (rule.isFact) return Set(rule)
      else return Set(rule) filter relationsHold map deleteAuxiliaryAtoms
    }
    println("rule: "+rule)
    val possibleVariableValues: Map[Variable, Set[Value]] = printTime("  possibleVariableValues") {
       inspect possibleVariableValues rule
    }
    val groundRules: Set[LarsRule] = printTime("  assignAndFilter") {
      Grounder.assignAndFilter(rule, possibleVariableValues)
    }
    groundRules
    //assignments map rule.assign filter relationsHold map deleteAuxiliaryAtoms
  }



  def relationsHold(rule: LarsRule): Boolean = {
    (rule.pos map (_.atom) filter isRelationAtom forall (a => holds(a))) &&
    (rule.neg map (_.atom) filter isRelationAtom forall (a => !holds(a)))
  }

  def deleteAuxiliaryAtoms(rule: LarsRule): LarsRule = {
    val corePosAtoms: Set[ExtendedAtom] = rule.pos filter (x => !isRelationAtom(x))
    val coreNegAtoms: Set[ExtendedAtom] = rule.neg filter (x => !isRelationAtom(x))
    UserDefinedLarsRule(rule.head.asInstanceOf[Atom],corePosAtoms,coreNegAtoms)
  }

  //note: creating new traits and case classes for 'RelationAtoms' would complicate things too much
  //i) we would have mixes of {GroundAtom,NonGroundAtom} with {RelationAtom, normal atoms}
  //ii) ensure that for the special predicate names, RelationAtom instances are generated
  //there, one still would have to make a pattern matching over predicate names somewhere/somehow
  //since this plays a role only at the level of grounding, we can do it here directly and keeping
  //the class hierarchy of atoms simple/uniform.
  def isRelationAtom(x: ExtendedAtom): Boolean = x match {
    case a:Atom =>  a.predicate.caption match {
      case "neq" => true
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

  def holds(relationAtom: Atom): Boolean = {
    if (!relationAtom.isGround())
      throw new RuntimeException("illegal use. relation atom must be ground when tested whether it holds.")

    val atom = relationAtom.asInstanceOf[GroundAtomWithArguments]
    val args = atom.arguments

    def i(idx: Int):Int = args(idx) match {
      case IntValue(k) => k
      case StringValue(s) => Integer.parseInt(s)
      case _ => throw new RuntimeException("unknown value "+args(idx)+" in non-ground relation atom "+atom)
    }

    relationAtom.predicate.caption match {
      case "neq" => args(0) != args(1)
      case "leq" => i(0) <= i(1)
      case "lt" => i(0) < i(1)
      case "geq" => i(0) >= i(1)
      case "gt" => i(0) > i(1)
      case "pow" => Math.pow(i(0),i(1)).toInt == i(2)
      case "mod" => (i(1) > 0) && (i(0) % i(1) == i(2))
      case "sum" => i(0) + i(1) == i(2)
      case "prod" => i(0) * i(1) == i(2)
      case _ => false
    }
  }

  def assignAndFilter(rule: LarsRule, possibleValuesPerVariable: Map[Variable,Set[Value]]): Set[LarsRule] = {
    val pairSingletonsPerVariable: Seq[Set[Set[(Variable,Value)]]] = printTime("    makePairedWithValueSingletons") {
      makePairedWithValueSingletons(possibleValuesPerVariable)
    }
    val preparedAssignments: Set[Set[(Variable, Value)]] = printTime("    pairSingletonsPerVariable") {
      pairSingletonsPerVariable.reduce((s1, s2) => cross(s1,s2))
    }
    println("    #preparedAssignments: "+preparedAssignments.size)
    val filteredGroundRules: Set[LarsRule] = printTime("    filteredGroundRules") {
      assignAndFilter(rule,preparedAssignments)
    }
    filteredGroundRules
  }

  // X -> { x1, x2 }
  // Y -> { y1, y2 }
  // Z -> { z1, z2 }
  // =>
  // [ { {(X,x1)}, {(X,x2)} }
  //   { {(Y,y1)}, {(Y,y2)} }
  //   { {(Z,z1)}, {(Z,z2)} } ]
  def makePairedWithValueSingletons(possibleValuesPerVariable: Map[Variable,Set[Value]]): Seq[Set[Set[(Variable,Value)]]] = {
    possibleValuesPerVariable map {
      case (variable,valueSet) => valueSet map (value => Set((variable, value))) // {{(X,x1)},{(X,x2)}}
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

  //keep those results of cross reduction where relation atoms in given rule hold
  def assignAndFilter(rule: LarsRule, assignments: Set[Set[(Variable, Value)]]): Set[LarsRule] = {
    assignments map {
      case assignment:Set[(Variable,Value)] => groundingIfRelationsHold(rule,assignment)
    } collect {
      case Some(groundRule) => groundRule
    }
  }

  def groundingIfRelationsHold(rule: LarsRule, bindings: Set[(Variable,Value)]): Option[LarsRule] = {
    val map: Map[Variable, Value] = bindings.toMap
    val assigned: LarsRule = rule.assign(new Assignment(map))
    if (relationsHold(assigned)) Some(deleteAuxiliaryAtoms(assigned))
    else None
  }

}

case class LarsProgramInspection(program: LarsProgram) {

  //ignore AtAtoms throughout

  val ruleCores: Set[LarsRule] = program.rules collect { case r if r.head.isInstanceOf[Atom] => reduceToCore(r) } toSet

  //delete negative body, window atoms, and auxiliary relation expressions (for arithmetic)
  //these are considered later in grounding itself, but not for finding variables
  def reduceToCore(rule: LarsRule): LarsRule = {
    val coreAtoms: Set[ExtendedAtom] = rule.pos collect { case a:Atom if !Grounder.isRelationAtom(a) => a }
    UserDefinedLarsRule(rule.head.asInstanceOf[Atom],coreAtoms)
  }

  val facts = ruleCores filter (_.isFact)
  val factAtoms = facts map (_.head)
  val factAtomPredicates = factAtoms map (_.atom.predicate)

  val groundFactAtoms = factAtoms collect { case a:GroundAtom => a }
  val groundFactAtomPredicates = groundFactAtoms map (_.predicate)
  //do not allow/consider facts to appear only non-ground! but fact atoms may appear non-ground in bodies of rules
  //however: for non ground, we must identify different variable names used in atoms!
  val nonGroundFactAtomPredicates = ruleCores flatMap (_.pos) collect {
    case b:Atom if groundFactAtomPredicates.contains(b.predicate) => b.predicate
  }

  val groundRuleHeadAtoms = ruleCores map (_.head) collect { case x: GroundAtom => x }
  val groundIntensionalAtoms = groundRuleHeadAtoms diff groundFactAtoms
  val groundIntensionalPredicates = groundIntensionalAtoms map (_.predicate)

  val nonGroundRuleHeadPredicates = ruleCores map (_.head) collect { case x: NonGroundAtom => x.predicate }
  val nonGroundIntensionalPredicates = nonGroundRuleHeadPredicates diff nonGroundFactAtomPredicates

  val justificationsOfNonGroundIntensionalPredicate: Map[Predicate, Set[LarsRule]] =
    ruleCores filter (r => !r.isFact && r.head.isInstanceOf[NonGroundAtom]) groupBy (_.head.atom.predicate)

  val groundFactAtomsPerPredicate: Map[Predicate, Set[GroundAtom]] = groundFactAtoms groupBy (_.predicate)
  val groundIntensionalAtomsPerPredicate: Map[Predicate, Set[GroundAtom]] = groundIntensionalAtoms groupBy (_.predicate)

  //("a" -> {a(x,y), a(z,w)})  ==>  ("a" -> (0 -> {x,z}, 1 -> {y,w}))
  def makeValueLookupMap(atomsPerPredicate: Map[Predicate, Set[GroundAtom]]): Map[Predicate,Map[Int,Set[Value]]] = {
    atomsPerPredicate mapValues { set =>
      set.map(_.asInstanceOf[AtomWithArgument].arguments) //{a(x,y), a(z,y)} ==> {(x,z), (y,w)}
        .flatMap(_.zipWithIndex) // ==> {(x,0), (y,1), (z,0), (w,1)}
        .groupBy { case (v,idx) => idx } //Map(1 -> {(y,1), (w,1)}, 0 -> {(x,0), (z,0)})
        .mapValues(_ map { case (v,idx) => v.asInstanceOf[Value] }) //Map(1 -> {y,w}, 0 -> {x,z})
    }
  }

  val groundFactAtomValuesLookup: Map[Predicate,Map[Int,Set[Value]]] = makeValueLookupMap(groundFactAtomsPerPredicate)
  val groundIntensionalValuesLookup: Map[Predicate,Map[Int,Set[Value]]] = makeValueLookupMap(groundIntensionalAtomsPerPredicate)

  //for every rule, group the atoms where each rule's variable appears in
  //restriction to those in positive body
  def makeAtomLookupMap(predicates: Set[Predicate]): Map[LarsRule,Map[Variable,Set[NonGroundAtom]]] = {
    def atomsPerVariable(rule:LarsRule): Map[Variable,Set[NonGroundAtom]] = {
      def atomsWithVar(v: Variable): Set[NonGroundAtom] = rule.pos collect {
        case atom:NonGroundAtom if (predicates contains atom.predicate) && atom.variables.contains(v) => atom
      }
      val variableOccurrences: Map[Variable,Set[NonGroundAtom]] = (rule.variables map (v => (v,atomsWithVar(v)))).toMap
      variableOccurrences
    }
    ruleCores filterNot (_.isGround) map (r => (r,atomsPerVariable(r))) toMap
  }

  val nonGroundFactAtomsPerVariableInRule = makeAtomLookupMap(nonGroundFactAtomPredicates)
  val nonGroundIntensionalAtomsPerVariableInRule = makeAtomLookupMap(nonGroundIntensionalPredicates)

  //
  //
  //

  def possibleVariableValues(rule: LarsRule): Map[Variable, Set[Value]] = {
    rule.variables map (v => (v,possibleValuesForVariable(rule,v))) toMap
  }

  def possibleValuesForVariable(rule: LarsRule, variable: Variable): Set[Value] = {

    val coreRule = reduceToCore(rule) //window variables and variables in negative body must occur elsewhere

    //per convention, a variable now cannot occur in a signal only.
    //we test first for fact atoms, then we try intentional atoms.

    val nonGroundFactAtoms = nonGroundFactAtomsPerVariableInRule(coreRule).getOrElse(variable,Set())

    //pick random, better use the one with minimal number of values
    if (nonGroundFactAtoms.nonEmpty) {
      val fact = nonGroundFactAtoms.head
      val idx = fact.arguments.indexOf(variable)
      return groundFactAtomValuesLookup(fact.predicate)(idx)
    }

    val nonGroundIntensionalAtoms = nonGroundIntensionalAtomsPerVariableInRule(coreRule).getOrElse(variable,Set())
    if (nonGroundIntensionalAtoms.isEmpty) {
      throw new RuntimeException("variable "+variable+" does not appear in a fact atom or intensional atom in rule "+coreRule)
    }

    // since the variable does not appear in a fact atom, we have to collect *all* values
    // that may match the intensional atom
    // (safety conditions just as for negation)

    nonGroundIntensionalAtoms flatMap { atom =>
      val idx = atom.arguments.indexOf(variable)
      lookupOrFindValuesForPredicateArg(atom.predicate,idx)
    }

  }

  var valuesForPredicateArg: Map[Predicate, Map[Int, Set[Value]]] = HashMap[Predicate, Map[Int, Set[Value]]]()

  def lookupOrFindValuesForPredicateArg(predicate: Predicate, argumentIdx: Int): Set[Value] = {

    var cacheForPredicate: Option[Map[Int, Set[Value]]] = valuesForPredicateArg.get(predicate)
    var valuesForArg: Option[Set[Value]] = None
    if (cacheForPredicate.isDefined) {
      valuesForArg = cacheForPredicate.get.get(argumentIdx)
      if (valuesForArg.isDefined) return valuesForArg.get
    }

    val values = findValuesForPredicateArg(predicate: Predicate, argumentIdx: Int)

    valuesForArg = Some(values)

    if (cacheForPredicate.isDefined) {
      cacheForPredicate = Some(cacheForPredicate.get + (argumentIdx -> values))
    } else {
      cacheForPredicate = Some(Map((argumentIdx -> values)))
    }

    valuesForPredicateArg = valuesForPredicateArg.updated(predicate,cacheForPredicate.get)

    values
  }

  def findValuesForPredicateArg(predicate: Predicate, argumentIdx: Int): Set[Value] = {

    //note: non-ground fact atoms (in rule bodies) can only match with their ground instances,
    //whereas intensional atoms may occur ground and non-ground, and the non-ground instances
    //must ground in fact atoms (per convention)

    if (factAtomPredicates contains predicate) {
      return groundFactAtomValuesLookup(predicate)(argumentIdx)
    }

    val groundIntensional: Set[Value] =
      if (groundIntensionalPredicates contains predicate) {
        groundIntensionalValuesLookup(predicate).getOrElse(argumentIdx,Set())
      } else {
        Set()
      }

    if (!nonGroundIntensionalPredicates.contains(predicate)) {
      return groundIntensional
    }

    val justifications: Set[LarsRule] = justificationsOfNonGroundIntensionalPredicate(predicate)

    //first consider head atoms where the given argument appears ground
    val tuple: (Set[LarsRule], Set[LarsRule]) = justifications partition { rule =>
      val arg = rule.head.asInstanceOf[AtomWithArgument].arguments(argumentIdx)
      arg.isInstanceOf[Value]
    }
    val justificationsWithValue = tuple._1
    val justificationsWithVariable = tuple._2

    val semiGroundIntensional: Set[Value] = justificationsWithValue map { rule =>
      val arg = rule.head.asInstanceOf[AtomWithArgument].arguments(argumentIdx)
      arg.asInstanceOf[Value]
    }

    //second, consider head atoms where the given argument appears non-ground.
    //there we have to retrieve values from the rule body, potentially recursively
    val variableSources: Set[(Predicate,Int)] = justificationsWithVariable flatMap { rule =>
      val variable = rule.head.asInstanceOf[AtomWithArgument].arguments(argumentIdx).asInstanceOf[Variable]
      val allSources: Set[(Predicate, Int)] = rule.pos collect { //neg ignored!
        case x: NonGroundAtom if x.variables.contains(variable) => (x.predicate, x.arguments.indexOf(variable))
      }
      val groundFactAtomSources: Set[(Predicate, Int)] = allSources filter { case (p,i) => groundFactAtomPredicates.contains(p) }
      if (groundFactAtomSources.nonEmpty) {
        groundFactAtomSources //base case - no need to look into potential others since they have to match the ground instances anyway
      } else {
        allSources //recursive case
      }
    }

    val nonGroundIntensional: Set[Value] = variableSources collect {
      //escaping infinite loops by pred!=predicate
      case (pred,idx) if (pred!=predicate) => lookupOrFindValuesForPredicateArg(pred,idx)
    } flatten

    groundIntensional ++ semiGroundIntensional ++ nonGroundIntensional
  }

}