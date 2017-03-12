package core.grounding

import core._
import core.asp.{NormalProgram, NormalRule}
import core.lars.{ExtendedAtom, HeadAtom, LarsProgram, LarsRule}

import scala.collection.immutable.HashMap

/**
  * Created by hb on 08.03.17.
  */
/*
 * Works for ASP and LARS without @
 */
case class StaticProgramInspection[TRule <: Rule[THead, TBody], THead <: HeadAtom, TBody <: ExtendedAtom](rules: Seq[TRule]) extends ProgramInspection[TRule,THead,TBody] {

  override def possibleVariableValues(rule: TRule): Map[Variable, Set[Value]] = {
    rule.variables map (v => (v, possibleValuesForVariable(rule, v))) toMap
  }

  //
  //
  //

  //ignore AtAtoms throughout

  val ruleCores: Set[TRule] = rules collect { case r if r.head.isInstanceOf[Atom] => reduceToCore(r) } toSet

  //delete negative body, window atoms, and auxiliary relation expressions (for arithmetic).
  //these are considered later in grounding itself, but not for finding variables
  def reduceToCore(rule: TRule): TRule = {
    val coreAtoms: Set[TBody] = rule.pos filterNot (_.isInstanceOf[RelationAtom])
    rule.from(rule.head, coreAtoms, rule.neg).asInstanceOf[TRule]
  }

  val facts = ruleCores filter (_.isFact)
  val factAtoms = facts map (_.head)
  val factAtomPredicates = factAtoms map (_.atom.predicate)

  val groundFactAtoms: Set[GroundAtom] = factAtoms collect { case a: GroundAtom => a }
  val groundFactAtomPredicates = groundFactAtoms map (_.predicate)


  val groundRuleHeadAtoms: Set[GroundAtom] = ruleCores map (_.head) collect { case x: GroundAtom => x }
  val groundIntensionalAtoms = groundRuleHeadAtoms diff groundFactAtoms
  val groundIntensionalPredicates = groundIntensionalAtoms map (_.predicate)

  val groundFactAtomValuesLookup: Map[Predicate, Map[Int, Set[Value]]] = makeValueLookupMap(groundFactAtoms)
  val groundIntensionalValuesLookup: Map[Predicate, Map[Int, Set[Value]]] = makeValueLookupMap(groundIntensionalAtoms)

  //{a(x,y),a(z,w)} ==>  ("a" -> (0 -> {x,z}, 1 -> {y,w}))
  def makeValueLookupMap(atoms: Set[GroundAtom]): Map[Predicate, Map[Int, Set[Value]]] = {
    //{a(x,y),a(z,w)} ==> ("a" -> {a(x,y), a(z,w)})
    val atomsPerPredicate: Map[Predicate, Set[GroundAtom]] = atoms groupBy (_.predicate)
    atomsPerPredicate mapValues { set =>
      set.map {
        case a: AtomWithArguments => a.arguments //{a(x,y), a(z,y)} ==> {(x,z), (y,w)}
        case _ => Seq()
      }
        .flatMap(_.zipWithIndex) // ==> {(x,0), (y,1), (z,0), (w,1)}
        .groupBy { case (v, idx) => idx } //Map(1 -> {(y,1), (w,1)}, 0 -> {(x,0), (z,0)})
        .mapValues( _ map { case (v, idx) => v.asInstanceOf[Value] }) //Map(1 -> {y,w}, 0 -> {x,z})
    }
  }

  /* non-ground stuff */

  val justificationsOfNonGroundIntensionalHeadPredicate: Map[Predicate, Set[TRule]] =
    ruleCores filter (r => !r.isFact && r.head.isInstanceOf[NonGroundAtom]) groupBy (_.head.atom.predicate)

  //do not allow/consider facts to appear only non-ground; but fact atoms may appear non-ground in bodies of rules
  //however: for non-ground, we must identify different variable names used in atoms
  val nonGroundFactAtomPredicates = ruleCores flatMap (_.pos) collect {
    case b: Atom if groundFactAtomPredicates.contains(b.predicate) => b.predicate
  }

  val nonGroundHeadPredicates = ruleCores map (_.head) collect { case x: NonGroundAtom => x.predicate }
  val nonGroundPosBodyPredicates = ruleCores flatMap (_.pos) collect { case x: NonGroundAtom => x.predicate }
  val nonGroundPredicates = nonGroundHeadPredicates union nonGroundPosBodyPredicates
  val intensionalPredicates = ruleCores map (_.head.atom.predicate)
  val nonGroundIntensionalPredicates = nonGroundPredicates intersect intensionalPredicates
  val nonGroundIntensionalPredicatesSansFacts = nonGroundIntensionalPredicates diff nonGroundFactAtomPredicates

  val nonGroundFactAtomsPerVariableInRule = makeAtomLookupMap(nonGroundFactAtomPredicates)
  val nonGroundIntensionalAtomsPerVariableInRule = makeAtomLookupMap(nonGroundIntensionalPredicatesSansFacts)

  //for every rule, group the atoms where each rule's variable appears in
  //restriction to those in positive body
  def makeAtomLookupMap(predicates: Set[Predicate]): Map[TRule, Map[Variable, Set[NonGroundAtom]]] = {
    def atomsPerVariable(rule: TRule): Map[Variable, Set[NonGroundAtom]] = {
      def atomsWithVar(v: Variable): Set[NonGroundAtom] = rule.pos collect {
        case atom: NonGroundAtom if (predicates contains atom.predicate) && atom.variables.contains(v) => atom
      }
      val variableOccurrences: Map[Variable, Set[NonGroundAtom]] = (rule.variables map (v => (v, atomsWithVar(v)))).toMap
      variableOccurrences
    }

    ruleCores filterNot (_.isGround) map (r => (r, atomsPerVariable(r))) toMap
  }

  //
  //
  //

  def possibleValuesForVariable(rule: TRule, variable: Variable): Set[Value] = {

    val coreRule = reduceToCore(rule) //window variables and variables in negative body must occur elsewhere //TODO move up

    //per convention, a variable now cannot occur in a signal only.
    //we test first for fact atoms, then we try intentional atoms.

    val nonGroundFactAtoms = nonGroundFactAtomsPerVariableInRule(coreRule).getOrElse(variable, Set())

    //pick random, better use the one with minimal number of values
    if (nonGroundFactAtoms.nonEmpty) {
      val factAtom = nonGroundFactAtoms.head
      val idx = factAtom.positionOf(variable)
      return groundFactAtomValuesLookup(factAtom.predicate)(idx)
    }

    val nonGroundIntensionalAtoms = nonGroundIntensionalAtomsPerVariableInRule(coreRule).getOrElse(variable, Set())
    if (nonGroundIntensionalAtoms.isEmpty) {
      throw new RuntimeException("rule " + coreRule + ": variable " + variable + " does not appear in a fact atom or intensional atom.")
    }

    // since the variable does not appear in a fact atom, we have to collect *all* values
    // that may match the intensional atom
    // (safety conditions just as for negation)

    nonGroundIntensionalAtoms flatMap { atom =>
      val idx = atom.positionOf(variable)
      lookupOrFindValuesForPredicateArg(atom.predicate, idx)
    }

  }

  var valuesForPredicateArg: Map[Predicate, Map[Int, Set[Value]]] = HashMap[Predicate, Map[Int, Set[Value]]]()

  var triedSources: Map[Predicate,Set[Predicate]] = HashMap[Predicate,Set[Predicate]]()

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

    valuesForPredicateArg = valuesForPredicateArg.updated(predicate, cacheForPredicate.get)

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
        groundIntensionalValuesLookup(predicate).getOrElse(argumentIdx, Set())
      } else {
        Set()
      }

    if (!nonGroundHeadPredicates.contains(predicate)) {
      return groundIntensional
    }

    // must find further values via potential derivations

    val justifications: Set[TRule] = justificationsOfNonGroundIntensionalHeadPredicate(predicate)

    //first consider head atoms where the given argument appears ground
    val tuple: (Set[TRule], Set[TRule]) = justifications partition { rule =>
      val arg = rule.head.asInstanceOf[AtomWithArguments].arguments(argumentIdx)
      arg.isInstanceOf[Value]
    }
    val justificationsWithValue = tuple._1
    val justificationsWithVariable = tuple._2

    val semiGroundIntensional: Set[Value] = justificationsWithValue map { rule =>
      val arg = rule.head.asInstanceOf[AtomWithArguments].arguments(argumentIdx)
      arg.asInstanceOf[Value]
    }

    //second, consider head atoms where the given argument appears non-ground.
    //there we have to retrieve values from the rule body, potentially recursively
    val variableSources: Set[(Predicate, Int)] = justificationsWithVariable flatMap { rule =>
      val variable = rule.head.asInstanceOf[AtomWithArguments].arguments(argumentIdx).asInstanceOf[Variable]
      val allSources: Set[(Predicate, Int)] = rule.pos collect {
        //neg deliberately ignored!
        case x: NonGroundAtom if x.positionOf(variable) >= 0 => (x.predicate, x.positionOf(variable))
      }
      val groundFactAtomSources: Set[(Predicate, Int)] = allSources filter { case (p, i) => groundFactAtomPredicates.contains(p) }
      if (groundFactAtomSources.nonEmpty) {
        groundFactAtomSources //base case - no need to look into potential others since they have to match the ground instances anyway
      } else {
        allSources //recursive case
      }
    }

    val nonGroundIntensional: Set[Value] = variableSources collect {
      case (pred, idx) if (!triedSources.getOrElse(predicate,Set()).contains(pred)) => {
        triedSources = triedSources + (predicate -> (triedSources.getOrElse(predicate,Set()) + pred))
        lookupOrFindValuesForPredicateArg(pred, idx)
      }
    } flatten

    groundIntensional ++ semiGroundIntensional ++ nonGroundIntensional
  }

}

object StaticProgramInspection {
  def forLars(program: LarsProgram): StaticProgramInspection[LarsRule, HeadAtom, ExtendedAtom] = StaticProgramInspection[LarsRule, HeadAtom, ExtendedAtom](program.rules)

  def forAsp(program: NormalProgram): StaticProgramInspection[NormalRule, Atom, Atom] = StaticProgramInspection[NormalRule, Atom, Atom](program.rules)
}