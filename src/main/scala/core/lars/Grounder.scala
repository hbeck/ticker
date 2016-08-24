package core.lars

import core._

import scala.collection.immutable.HashMap


/**
  * applicable only to lars programs without "@"
  *
  * Created by hb on 8/21/16.
  */
case class Grounder(program: LarsProgram) {

  lazy val inspect = LarsProgramInspection(program)
  lazy val groundRules = program.rules flatMap Grounder.ground(inspect)
  lazy val groundProgram = LarsProgram(groundRules)

}

object Grounder {

  def ground(inspect: LarsProgramInspection)(rule: LarsRule): Set[LarsRule] = {
    if (rule isGround) return Set(rule)
    val possibleVariableValues: Map[Variable, Set[Value]] = inspect possibleVariableValues rule
    val assignments: Set[Assignment] = Grounder.createAssignments(possibleVariableValues)
    assignments map (rule.assign(_))
  }

  def createAssignments(possibleValuesPerVariable: Map[Variable,Set[Value]]): Set[Assignment] = {
    val pairSingletonsPerVariable: Seq[Set[Set[(Variable,Value)]]] = makePairedWithValueSingletons(possibleValuesPerVariable)
    val preparedAssignments: Set[Set[(Variable, Value)]] = pairSingletonsPerVariable.reduce((s1, s2) => cross(s1,s2))
    preparedAssignments map (set => Assignment(set.toMap))
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

}

case class LarsProgramInspection(program: LarsProgram) {

  //ignore AtAtoms throughout

  val rules: Set[BasicLarsRule] = program.rules collect { case r if r.head.isInstanceOf[Atom] => asBasicRule(r) } toSet

  def asBasicRule(rule: LarsRule): BasicLarsRule = {
    UserDefinedBasicLarsRule(rule.head.asInstanceOf[Atom],rule.pos,rule.neg)
  }

  val facts = rules filter (_.isFact)
  val factAtoms = facts map (_.head)
  val factAtomPredicates = factAtoms map (_.predicate)

  val groundFacts = rules filter (r => r.isFact && r.head.isInstanceOf[GroundAtom])
  val groundFactAtoms = groundFacts map (_.head.asInstanceOf[GroundAtom])
  val groundFactAtomPredicates = groundFactAtoms map (_.predicate)
  //do not allow/consider facts to appear only non-ground! but fact atoms may appear non-ground in bodies of rules
  //however: for non ground, we must identify different variable names used in atoms!
  val nonGroundFactAtomPredicates = rules flatMap (_.body) collect {
    case b:Atom if groundFactAtomPredicates.contains(b.predicate) => b.predicate
  }

  val groundRuleHeadAtoms = rules map (_.head) collect { case x: GroundAtom => x }
  val groundIntensionalAtoms = groundRuleHeadAtoms diff groundFactAtoms
  val groundIntensionalPredicates = groundIntensionalAtoms map (_.predicate)

  val nonGroundRuleHeadPredicates = rules map (_.head) collect { case x: NonGroundAtom => x.predicate }
  val nonGroundIntensionalPredicates = nonGroundRuleHeadPredicates diff nonGroundFactAtomPredicates

  val justificationsOfNonGroundIntensionalPredicate: Map[Predicate, Set[BasicLarsRule]] =
    rules filter (r => !r.isFact && r.head.isInstanceOf[NonGroundAtom]) groupBy (_.head.predicate)

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
  def makeAtomLookupMap(predicates: Set[Predicate]): Map[BasicLarsRule,Map[Variable,Set[NonGroundAtom]]] = {
    def atomsPerVariable(rule:BasicLarsRule): Map[Variable,Set[NonGroundAtom]] = {
      def atomsWithVar(v: Variable): Set[NonGroundAtom] = rule.pos collect {
        case atom:NonGroundAtom if (predicates contains atom.predicate) && atom.variables.contains(v) => atom
      }
      val variableOccurrences: Map[Variable,Set[NonGroundAtom]] = (rule.variables map (v => (v,atomsWithVar(v)))).toMap
      variableOccurrences
    }
    rules filterNot (_.isGround) map (r => (r,atomsPerVariable(r))) toMap
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

    val r = asBasicRule(rule)

    //per convention, a variable now cannot occur in a signal only.
    //we test first for fact atoms, then we try intentional atoms.

    val nonGroundFactAtoms = nonGroundFactAtomsPerVariableInRule(r).getOrElse(variable,Set())

    //pick random, better use the one with minimal number of values
    if (nonGroundFactAtoms.nonEmpty) {
      val fact = nonGroundFactAtoms.head
      val idx = fact.arguments.indexOf(variable)
      return groundFactAtomValuesLookup(fact.predicate)(idx)
    }

    val nonGroundIntensionalAtoms = nonGroundIntensionalAtomsPerVariableInRule(r).getOrElse(variable,Set())
    if (nonGroundIntensionalAtoms.isEmpty) {
      throw new RuntimeException("variable "+variable+" does not appear in "+r+" in an fact atom or intensional atom")
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

    val justifications: Set[BasicLarsRule] = justificationsOfNonGroundIntensionalPredicate(predicate)

    val variableSources: Set[(Predicate,Int)] = justifications flatMap { rule =>
      val variable = rule.head.asInstanceOf[AtomWithArgument].arguments(argumentIdx).asInstanceOf[Variable]
      val allSources: Set[(Predicate, Int)] = rule.body collect {
        case x: NonGroundAtom if x.variables.contains(variable) => (x.predicate, x.arguments.indexOf(variable))
      }
      val groundFactAtomSources: Set[(Predicate, Int)] = allSources filter { case (p,i) => groundFactAtomPredicates.contains(p) }
      if (groundFactAtomSources.nonEmpty) {
        groundFactAtomSources //base case - no need to look into potential others since they have to match the ground instances anyway
      } else {
        allSources //recursive case
      }
    }

    val nonGroundIntensional: Set[Value] = variableSources collect { case (pred,idx) if (pred!=predicate) => lookupOrFindValuesForPredicateArg(pred,idx) } flatten

    groundIntensional ++ nonGroundIntensional
  }

}