package core.lars


/**
  * applicable only to lars programs without "@"
  *
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
    val possibleVariableValues: Map[Variable, Set[Value]] = inspect possibleVariableValues rule
    val assignments: Set[Assignment] = createAssignments(possibleVariableValues)
    assignments map (rule.assign(_))
  }

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

  //ignore AtAtoms throughout

  val rules: Set[BasicLarsRule] = program.rules collect { case r if r.head.isInstanceOf[Atom] => asBasic(r) } toSet

  def asBasic(rule: LarsRule): BasicLarsRule = {
    UserDefinedBasicLarsRule(rule.head.asInstanceOf[Atom],rule.pos,rule.neg)
  }

  val facts = rules filter (_.isFact)
  val factAtoms = facts map (_.head)
  val factAtomPredicates = factAtoms map (_.predicate)

  val groundFacts = rules filter (r => r.isFact && r.head.isInstanceOf[GroundAtom])
  val groundFactAtoms = groundFacts map (_.head.asInstanceOf[GroundAtom])
  val groundFactAtomPredicates = groundFactAtoms map (_.predicate)
  //do not allow/consider non-ground facts! but fact atoms may appear non-ground in bodies of rules
  //however: for non ground, we must identify different variable names used in atoms!
  val nonGroundFactPredicates = rules flatMap (_.body) collect {
    case b:Atom if groundFactAtomPredicates.contains(b.predicate) => b.predicate
  }

  val groundRuleHeadAtoms = rules map (_.head) collect { case x: GroundAtom => x }
  val groundIntensionalAtoms = groundRuleHeadAtoms diff groundFactAtoms

  val nonGroundRuleHeadPredicates = rules map (_.head) collect { case x: NonGroundAtom => x.predicate }
  val nonGroundIntensionalPredicates = nonGroundRuleHeadPredicates diff nonGroundFactPredicates

  val justificationsOfNonGroundIntensionalPredicate: Map[Predicate, Set[BasicLarsRule]] =
    rules filter (r => !r.isFact && r.head.isInstanceOf[NonGroundAtom]) groupBy (_.head.predicate)

  val groundFactAtomsPerPredicate: Map[Predicate, Set[GroundAtom]] = groundFactAtoms groupBy (_.predicate)
  val groundIntensionalAtomsPerPredicate: Map[Predicate, Set[GroundAtom]] = groundIntensionalAtoms groupBy (_.predicate)

  //("a" -> {a(x,y), a(z,w)})  ==>  ("a" -> (0 -> {x,z}, 1 -> {y,w}))
  def makeLookupMap(atomsPerPredicate: Map[Predicate, Set[GroundAtom]]): Map[Predicate,Map[Int,Set[Value]]] = {
    atomsPerPredicate mapValues { set =>
      set.map(_.asInstanceOf[AtomWithArgument].arguments) //{a(x,y), a(z,y)} ==> {(x,z), (y,w)}
        .flatMap(_.zipWithIndex) // ==> {(x,0), (y,1), (z,0), (w,1)}
        .groupBy(_._2) //Map(1 -> {(y,1), (w,1)}, 0 -> {(x,0), (z,0)})
        .mapValues(_ map (pair => pair._1.asInstanceOf[Value])) //Map(1 -> {y,w}, 0 -> {x,z})
    }
  }

  val groundFactAtomValuesLookup: Map[Predicate,Map[Int,Set[Value]]] = makeLookupMap(groundFactAtomsPerPredicate)
  val groundIntensionalValuesLookup: Map[Predicate,Map[Int,Set[Value]]] = makeLookupMap(groundIntensionalAtomsPerPredicate)

  //rule parts with variables
  val nonGroundIntensionalAtomsPerVariable: Map[BasicLarsRule, Map[Variable,Set[NonGroundAtom]]]
  val nonGroundFactAtomsPerVariable: Map[BasicLarsRule,Map[Variable,Set[NonGroundAtom]]] //extensional atoms for which ground fact exists per assumption

  //

  def possibleVariableValues(rule: LarsRule): Map[Variable, Set[Value]] = {
    rule.variables() map (v => (v,possibleValuesForVariable(rule,v))) toMap
  }

  def possibleValuesForVariable(rule: LarsRule, variable: Variable): Set[Value] = {

    val r = asBasic(rule)

    //per convention, a variable now cannot occur in a signal only.
    //we test first for fact atoms, then we try intentional atoms.

    val nonGroundFactAtoms = nonGroundFactAtomsPerVariable(r).getOrElse(variable,Set())
    //pick random, better use the one with minimal number of values
    if (!nonGroundFactAtoms.isEmpty) {
      val fact = nonGroundFactAtoms.head
      val idx = fact.arguments.indexOf(variable)
      return groundFactAtomValuesLookup(fact.predicate)(idx)
    }

    val nonGroundIntensionalAtoms = nonGroundIntensionalAtomsPerVariable(r).getOrElse(variable,Set())
    if (nonGroundIntensionalAtoms.isEmpty) {
      throw new RuntimeException("variable "+variable+" appears in "+r+" only in a signal.")
    }

    // since the variable does not appear in a fact atom, we have to collect *all* values
    // that may match the intensional atom
    // (safety conditions just as for negation)

    //TODO all, initially only first:
    val intAtom: NonGroundAtom = nonGroundIntensionalAtoms.head
    val arg = intAtom.arguments.indexOf(variable)
    findValuesForPredicateArg(intAtom.predicate,arg)

  }

  def findValuesForPredicateArg(predicate: Predicate, arg: Int): Set[Value] = {

    //note: non-ground fact atoms (in rule bodies) can only match with their ground instances,
    //whereas intensional atoms may occur ground and non-ground, and the non-ground instances
    //must ground in fact atoms (per convention)

    if (factAtomPredicates contains predicate) {
      return groundFactAtomValuesLookup(predicate)(arg)
    }

    val groundIntensional: Set[Value] = groundIntensionalValuesLookup(predicate).getOrElse(arg,Set())
    if (!nonGroundIntensionalPredicates.contains(predicate)) {
      return groundIntensional
    }

    val justifications: Set[BasicLarsRule] = justificationsOfNonGroundIntensionalPredicate(predicate)

    val variableSources: Set[(Predicate,Int)] = justifications flatMap { rule =>
      val variable = rule.head.asInstanceOf[AtomWithArgument].arguments(arg).asInstanceOf[Variable]
      val allSources: Set[(Predicate, Int)] = rule.body collect {
        case x: NonGroundAtom if x.variables().contains(variable) =>
          (x.predicate, x.arguments.indexOf(variable))
      }
      val groundFactAtomSources: Set[(Predicate, Int)] = allSources filter { case (p,i) => groundFactAtomPredicates.contains(p) }
      if (groundFactAtomSources.nonEmpty) {
        groundFactAtomSources //base case - no need to look into potential others since they have to match the ground instances anyway
      } else {
        allSources //recursive case
      }
    }

    val nonGroundIntensional: Set[Value] = variableSources flatMap { case (predicate,arg) => findValuesForPredicateArg(predicate,arg) }

    groundIntensional ++ nonGroundIntensional

  }

  */

}