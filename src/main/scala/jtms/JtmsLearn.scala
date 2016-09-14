package jtms

import core._
import core.asp.{NormalProgram, NormalRule}

import scala.collection.immutable.HashSet
import scala.util.Random

object JtmsLearn {

  def apply(P: NormalProgram): JtmsLearn = {
    val net = new JtmsLearn()
    net.shuffle = false
    P.rules foreach net.add
    net
  }

}

/**
  * Refinement of JtmsGreedy that learns to avoid bad choices.
  *
  */
class JtmsLearn(override val random: Random = new Random()) extends JtmsGreedy {

  shuffle = true

  /*
  stable:
  */
  /*
  override def updateGreedy(atoms: Set[Atom]) {
    atoms foreach setUnknown
    //test avoidance map before determining further consequences:
    selectNextAtom() //TODO this place in particular the need to distinguish determined atoms and choice atoms
    if (selectedAtom.isEmpty) {
      //atomsNeedingSupp() foreach setUnknown
      super.invalidateModel()
    } else {
      saveState()
    }
    while (hasUnknown) {
      unknownAtoms foreach findStatus //TODO could limit to transitive consequences of previously set head atom
      selectNextAtom()
      selectedAtom match {
        case Some(atom) => {
          chooseStatusGreedy(atom)
          saveState()
        }
        case None => if (hasUnknown) throw new IncrementalUpdateFailureException()
      }
    }
    //reset for next update iteration
    resetSavedState
  }
  */

  override def updateGreedy(atoms: Set[Atom]) {
    atoms foreach setUnknown
    //test avoidance map before determining further consequences:
    var firstLoop = true
    while (hasUnknown) {
      unknownAtoms foreach findStatus
      selectNextAtom()
      selectedAtom match {
        case Some(atom) => {
          chooseStatusGreedy(atom)
          saveState()
        }
        case None => {
          if (hasUnknown) {
            if (firstLoop) {
              super.invalidateModel()
            } else {
              throw new IncrementalUpdateFailureException()
            }
          }
        }
      }
      firstLoop = false
    }
    //reset for next update iteration
    resetSavedState
  }

  /*
  override def update(atoms: Set[Atom]) {

    if (recordChoiceSeq) choiceSeq = Seq[Atom]()
    if (recordStatusSeq) statusSeq = Seq[(Atom,Status,String)]()

    try {
      updateLearnNew(atoms)

      checkJtmsSemantics()
      checkSelfSupport()
      checkConsistency()
    } catch {
      case e:IncrementalUpdateFailureException => {
        invalidateModel()
      }
    }

  }

  def updateLearnNew(atoms: Set[Atom]) {
    atoms foreach setUnknown
    //test avoidance map before determining further consequences:
    selectNextChoiceAtomNew() //?
    if (selectedAtom.isEmpty) {
      atomsNeedingSupp() foreach setUnknown
    } else {
      //saveLightweightState()
      saveState()
    }
    while (hasUnknown) {
      unknownAtoms foreach findStatus
      selectNextChoiceAtomNew() //avoid only when no next atom exists, not nec. choice atom. do we ensure that?
      selectedAtom match {
        case Some(atom) => {
          chooseStatusGreedy(atom)
          //saveLightweightState()
          saveState()
        }
        case None => {
          selectNextInferrableAtomNew() match {
            case Some(atom) => {
              chooseStatusGreedy(atom)
            }
            case None => if (hasUnknown)
              throw new IncrementalUpdateFailureException()
          }
        }
      }
    }
    //reset for next update iteration
    //resetSavedLightweightState()
    resetSavedState()
  }
  */

  case class PartialState(support: Map[Atom, Long], stateHash: Long) {
    //  case class PartialState(status: Map[Atom, Status], support: Map[Atom, Set[Atom]]) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("State[").append("\n\t\tstatus: ").append(stateHash).append("\n\t\tsupport: ").append(support).append("]")
      sb.toString
    }

    private lazy val precomputedHash = scala.runtime.ScalaRunTime._hashCode(PartialState.this)

    override def hashCode(): Int = precomputedHash

    //    override lazy val hashCode(): Int = scala.runtime.ScalaRunTime._hashCode(UserDefinedAspRule.this)
  }

  def saveState() {
    prevState = state
    prevSelectedAtom = selectedAtom
  }

//  def saveLightweightState(): Unit = {
//    prevLightweightState = lightweightState
//    prevSelectedAtom = selectedAtom
//  }

  def resetSavedState(): Unit = {
    prevState = None
    prevSelectedAtom = None
  }

//  def resetSavedLightweightState(): Unit = {
//    prevLightweightState = None
//    prevSelectedAtom = None
//  }

  var selectedAtom: Option[Atom] = None
  var prevSelectedAtom: Option[Atom] = None

  var state: Option[PartialState] = None
  var prevState: Option[PartialState] = None
  //var lightweightState: Option[PartialState] = None
  //var prevLightweightState: Option[PartialState] = None

  case class PrecomputedHashCodeOfHashSet(rules: HashSet[NormalRule], incrementalHash: Long = IncrementalHashCode.emptyHash) {

    def contains(rule: NormalRule) = rules.contains(rule)

    def +(rule: NormalRule) = PrecomputedHashCodeOfHashSet(rules + rule, IncrementalHashCode.addHashCode(incrementalHash, rule))

    def -(rule: NormalRule) = PrecomputedHashCodeOfHashSet(rules - rule, IncrementalHashCode.removeHashCode(incrementalHash, rule))

    //    private val precomputedHash = scala.runtime.ScalaRunTime._hashCode(PrecomputedHashCodeOfHashSet.this)
    private val precomputedHash = incrementalHash.hashCode()

    override def hashCode(): Int = precomputedHash

    override def toString(): String = "preHash[" + incrementalHash + "]: " + rules.toString

  }

  class AllRulesTabu() extends FrequencyCount[PrecomputedHashCodeOfHashSet] {

    var triggerCount = 10000

    var ruleMap: Map[PrecomputedHashCodeOfHashSet, CurrentRulesTabu] = Map.empty.withDefaultValue(new CurrentRulesTabu())
    //var ruleMapLightweight: Map[PrecomputedHashCodeOfHashSet, CurrentRulesLightweightTabu] = Map.empty.withDefaultValue(new CurrentRulesLightweightTabu())

    var stateRules: PrecomputedHashCodeOfHashSet = PrecomputedHashCodeOfHashSet(HashSet())
    var currentRulesTabu = new CurrentRulesTabu()
    //var currentRulesLightweightTabu = new CurrentRulesLightweightTabu()

    def add(rule: NormalRule): Unit = {
      if (!stateRules.contains(rule)) {
        stateRules = stateRules + rule
        updateAfterRuleChange()
      }
    }

    def remove(rule: NormalRule): Unit = {
      if (stateRules.contains(rule)) {
        stateRules = stateRules - rule
        updateAfterRuleChange()

        ruleMap = cleanupState(ruleMap)
      }
    }

    def updateAfterRuleChange(): Unit = {
      if (ruleMap.contains(stateRules)) {
        currentRulesTabu = ruleMap(stateRules)
        recordUsage(stateRules)
      } else {
        currentRulesTabu = new CurrentRulesTabu()
        ruleMap = ruleMap.updated(stateRules, currentRulesTabu)
        //currentRulesLightweightTabu = new CurrentRulesLightweightTabu()
        //ruleMapLightweight = ruleMapLightweight.updated(stateRules, currentRulesLightweightTabu)
      }
    }


    def avoid(state: PartialState, atomToAvoid: Atom): Unit = {
      currentRulesTabu.save(state, atomToAvoid)
    }

//    def avoidLightweight(state: Long, atomToAvoid: Atom): Unit = {
//      currentRulesLightweightTabu.save(state,atomToAvoid)
//    }

    def atomsToAvoid(): Set[Atom] = {
      currentRulesTabu.atomsToAvoid()
    }

//    def atomsToAvoidLightWeight(): Set[Atom] = {
//      currentRulesLightweightTabu.atomsToAvoid()
//    }

    override def toString(): String = {
      val sb = new StringBuilder()
      for ((ruleSet, tb) <- ruleMap) {
        sb.append("rules: ").append(ruleSet)
        for ((state, atoms) <- tb.avoidanceMap) {
          sb.append("\n\tin ").append(state)
          sb.append("\n\tavoid ").append(atoms)
        }
        sb.append("\n")
      }
      sb.toString
    }

  }


  class CurrentRulesTabu() extends FrequencyCount[PartialState] {
    var triggerCount: Int = 100

    var avoidanceMap: Map[PartialState, Set[Atom]] = Map()

    def save(state: PartialState, atomToAvoid: Atom): Unit = {
      val set: Set[Atom] = avoidanceMap.getOrElse(state, Set()) + atomToAvoid
      avoidanceMap = avoidanceMap.updated(state, set)
    }

    def atomsToAvoid() = {
      avoidanceMap = cleanupState(avoidanceMap)
      recordUsage(state.get)
      avoidanceMap.getOrElse(state.get, Set[Atom]())
    }
  }

//  class CurrentRulesLightweightTabu() {
//    var avoidanceMap = new HashMap[PartialState, Set[Atom]]
//
//    def save(state: PartialState, atomToAvoid: Atom): Unit = {
//      val set: Set[Atom] = avoidanceMap.getOrElse(state, Set()) + atomToAvoid
//      avoidanceMap = avoidanceMap.updated(state, set)
//    }
//
//    def atomsToAvoid() = avoidanceMap.getOrElse(lightweightState.get, Set[Atom]())
//  }

  val tabu = new AllRulesTabu()

  override def register(rule: NormalRule): Boolean = {
    val newRule = super.register(rule)
    if (!newRule) {
      return false
    } else if (dataIndependentRule(rule)) {
      tabu.add(rule)
    }
    true
  }

  override def unregister(rule: NormalRule): Boolean = {
    val ruleExisted = super.unregister(rule)
    if (!ruleExisted) {
      return false
    } else if (dataIndependentRule(rule)) {
      tabu.remove(rule)
    }
    true
  }

  //note that in this case, invalidateModel is not supposed to be called from outside!
  override def invalidateModel(): Unit = {

    if (selectedAtom.isDefined) {
      tabu.avoid(state.get, selectedAtom.get)
      //updateAvoidanceMap(this.state.get,selectedAtom.get)
    }

    super.invalidateModel()
  }

  def stateSnapshot(): Option[PartialState] = {
    //    val filteredStatus = status filter { case (atom,status) => isStateAtom(atom) }
    val currentStateAtoms = stateAtoms
    //    val filteredStatus = status filterKeys currentStateAtoms.contains
    //    val collectedSupp = supp collect { case (atom,set) if isStateAtom(atom) => (atom,set filter (!extensional(_))) }
    //    val collectedSupp = supp filterKeys currentStateAtoms.contains collect { case (atom, set) => (atom, set diff extensionalAtoms) }
    val collectedSupp = __suppHash filterKeys currentStateAtoms.contains

    //     val recomputedSupp =  supp filterKeys currentStateAtoms.contains collect { case (atom, set) => (atom,IncrementalHashCode.hash(set diff extensionalAtoms)) }

    Some(PartialState(collectedSupp, __stateHash))
  }

  def stateAtoms = (inAtoms union outAtoms) diff signals

  def stateSnapshotNew(): Option[PartialState] = {
    val currentStateAtoms = stateAtomsNew
    val collectedSupp = __suppHash filterKeys currentStateAtoms.contains
    //Some(PartialState(collectedSupp, __lightweightStateHash))
    Some(PartialState(collectedSupp, __stateHash))
  }

  def stateAtomsNew = __choiceAtoms diff unknownAtoms()

  //skip signals! - for asp they are irrelevant, for tms they change based on time - no stable basis
  //def isStateAtom(a: Atom): Boolean = (status(a) == in || status(a) == out) && !isSignal(a)

  /*
  def selectNextChoiceAtomNew(): Unit = {
    //lightweightState = stateSnapshotNew()
    selectedAtom = None
    state = stateSnapshotNew()

    val atomSet = unknownChoiceAtoms()
    val atoms = if (shuffle && atomSet.size > 1) (random.shuffle(atomSet.toSeq)) else atomSet

    if (atoms.isEmpty) {
//      if (selectedAtom.isEmpty && prevLightweightState.isDefined) {
//        tabu.avoidLightweight(prevLightweightState.get, prevSelectedAtom.get)
//      }
      if (selectedAtom.isEmpty && prevState.isDefined) {
        tabu.avoid(prevState.get, prevSelectedAtom.get)
      }
      return
    }

    val tabuAtoms = tabu.atomsToAvoid()

    selectedAtom = atoms find (!tabuAtoms.contains(_))

//    if (selectedAtom.isEmpty && prevLightweightState.isDefined) {
//      tabu.avoid(prevLightweightState.get, prevSelectedAtom.get)
//    }
    if (selectedAtom.isEmpty && prevState.isDefined) {
      tabu.avoid(prevState.get, prevSelectedAtom.get)
    }
  }
  */

  /*
  def selectNextInferrableAtomNew(): Option[Atom] = {

    val atomSet = unknownAtoms diff unknownChoiceAtoms() diff signals
    val atoms = if (shuffle && atomSet.size > 1) (random.shuffle(atomSet.toSeq)) else atomSet

    if (atoms.isEmpty) return None

    atoms.headOption
  }
  */

  def selectNextAtom(): Unit = {

    state = stateSnapshot()

    val atomSet = (unknownAtoms diff signals) //filter (a => a.predicate.caption == "bit" || a.predicate.caption == "xx1") //TODO
    val atoms = if (shuffle && atomSet.size > 1)
        (random.shuffle(atomSet.toSeq))
      else
        atomSet
        //atomSet.toSeq.sortWith((r1,r2) => posValid(r1))

    if (atoms.isEmpty) return

    // TODO performance: find iterates over too many atoms - dict?
    val tabuAtoms = tabu.atomsToAvoid()

    /*
    // <new160826>
    val nonContradictionAtoms = atoms filterNot (_.isInstanceOf[ContradictionAtom])
    if (nonContradictionAtoms.isEmpty) {
      selectedAtom = atoms find (!tabuAtoms.contains(_))
    } else {
      selectedAtom = nonContradictionAtoms find (!tabuAtoms.contains(_))
      if (selectedAtom.isEmpty) {
        selectedAtom = (atoms filter (_.isInstanceOf[ContradictionAtom])) find (!tabuAtoms.contains(_))
      }
    }
    //</new160826>
    */

    selectedAtom = atoms find (!tabuAtoms.contains(_))
    //    selectedAtom = atoms find (!tabu.atomsToAvoid().contains(_))

    if (selectedAtom.isEmpty && prevState.isDefined) {
      tabu.avoid(prevState.get, prevSelectedAtom.get)
    }

    /*
    if (doForceChoiceOrder) {
      val maybeAtom: Option[Atom] = forcedChoiceSeq find (status(_) == unknown)
      if (maybeAtom.isDefined) {
        selectedAtom = maybeAtom
        return
      }
    }
    */

  }

  def printAvoidanceMap(): Unit = {

    val map = tabu.currentRulesTabu.avoidanceMap

    if (map.isEmpty) return

    val n = Math.min(5,map.size)
    println(n+"/"+map.size+" entries of avoidance map:")

    (map take n) foreach { case (state, atoms) =>
      println("state hash: " + state)
      println("avoid atoms: " + atoms)
    }
  }

}

trait FrequencyCount[THash] {
  val typeName = this.getClass.toGenericString

  var counter: Map[THash, MutableCounter] = Map.empty

  var doCleanup = true

  var cleanupCounter = 0

  var thresholdPercent = 0.4
  var triggerCount: Int

  var maxValue = 0

  def cleanupState[TValue](stateToCleanup: Map[THash, TValue]): Map[THash, TValue] = {
    if (doCleanup) {
      cleanupCounter = cleanupCounter + 1
      if (cleanupCounter % triggerCount == 0) {
        // throw away the bottom x %. Value chosen so all test cases run through :)
        val threshold = maxValue * thresholdPercent
        val stateToKeep = counter.filter { case (_, c) => c.value > threshold } keySet

        //Console.out.println(f"Cleanup $typeName with threshold $threshold, keeping ${stateToKeep.size} items")

        counter = counter filterKeys stateToKeep
        return stateToCleanup filterKeys stateToKeep
      }
    }
    stateToCleanup
  }

  def recordUsage(value: THash): Unit = {
    if (doCleanup) {
      val count = counter.get(value)
      if (count.isDefined) {
        val value = count.get.increment()
        maxValue = Math.max(maxValue, value)
      } else {
        counter = counter.updated(value, new MutableCounter)
      }
    }
  }

  class MutableCounter {
    var value: Int = 1

    def increment() = {
      value = value + 1
      value
    }
  }


}