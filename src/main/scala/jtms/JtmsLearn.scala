package jtms

import core._
import core.asp.{NormalRule, NormalProgram}

import scala.collection.immutable.{HashSet, HashMap}
import scala.util.Random

object JtmsLearn {

  def apply(P: NormalProgram): JtmsLearn = {
    val net = new JtmsLearn()
    P.rules foreach net.add
    net
  }

}

/**
 * Refinement of JtmsGreedy that learns to avoid bad choices.
 *
 */
class JtmsLearn(override val random: Random = new Random()) extends JtmsGreedy {

  /*

  case class State(rules: scala.collection.immutable.Set[NormalRule], status: Map[Atom, Status], support: Map[Atom, scala.collection.immutable.Set[Atom]]) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("State[\n").append("  rules:  ").append(rules).append("\n")
          .append("  status:  ").append(status).append("\n  support: ").append(support).append("]")
      sb.toString
    }
  }

  */

  case class PartialState(status: Map[Atom, Status], support: Map[Atom, scala.collection.immutable.Set[Atom]]) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("State[\n").append("  status:  ").append(status).append("\n  support: ").append(support).append("]")
      sb.toString
    }
  }
  
  def saveState() {
    prevState = state
    prevSelectedAtom = selectedAtom
  }
  
  def resetSavedState(): Unit = {
    prevState = None
    prevSelectedAtom = None
  }

  override def updateGreedy(atoms: Set[Atom]) {
    atoms foreach setUnknown
    //test avoidance map before determining further consequences
    selectNextAtom()
    if (selectedAtom.isEmpty) {
      atomsNeedingSupp() foreach setUnknown
    } else {
      saveState
    }
    while (hasUnknown) {
      unknownAtoms foreach findStatus
      selectNextAtom()
      selectedAtom match {
        case Some(atom) => {
          chooseStatusGreedy(atom)
          saveState
        }
        case None => if (hasUnknown) throw new IncrementalUpdateFailureException()
      }
    }
    //reset for next update iteration
    resetSavedState
  }

  var selectedAtom: Option[Atom] = None
  var prevSelectedAtom: Option[Atom] = None
  var state: Option[PartialState] = None
  var prevState: Option[PartialState] = None

  //var avoidanceMap = new HashMap[PartialState,Set[Atom]]()

  class AllRulesTabu() {

    var ruleMap: Map[Set[NormalRule],CurrentRulesTabu] = Map.empty.withDefaultValue(new CurrentRulesTabu())

    var stateRules: Set[NormalRule] = new HashSet[NormalRule]()
    var currentRulesTabu = new CurrentRulesTabu()

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
      }
    }

    def updateAfterRuleChange(): Unit = {
      if (ruleMap.contains(stateRules)) {
        currentRulesTabu = ruleMap(stateRules)
      } else {
        currentRulesTabu = new CurrentRulesTabu()
        ruleMap = ruleMap.updated(stateRules,currentRulesTabu)
      }
    }

    def avoid(state: PartialState, atomToAvoid: Atom): Unit = {
      currentRulesTabu.save(state,atomToAvoid)
    }
    def atomsToAvoid(): Set[Atom] = {
      currentRulesTabu.atomsToAvoid()
    }

    override def toString(): String = {
      val sb = new StringBuilder()
      for ((ruleSet,tb) <- ruleMap) {
        sb.append("rules: ").append(ruleSet).append("\n")
        for ((state,atoms) <- tb.avoidanceMap) {
          sb.append("  state: ").append(state)
          sb.append("  avoid: ").append(atoms)
        }
      }
      sb.toString
    }
  }

  class CurrentRulesTabu() {
    var avoidanceMap = new HashMap[PartialState,Set[Atom]]
    def save(state: PartialState, atomToAvoid: Atom): Unit = {
      val set: Set[Atom] = avoidanceMap.getOrElse(state,Set()) + atomToAvoid
      avoidanceMap = avoidanceMap.updated(state,set)
    }
    def atomsToAvoid() = avoidanceMap.getOrElse(state.get,Set[Atom]())
  }

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

//  def updateAvoidanceMap(state: PartialState, avoidAtom: Atom): Unit = {
////    println("\nupdateAvoidanceMap:")
////    println(state)
////    println("avoid atom: "+avoidAtom+"\n")
//    if (avoidanceMap contains state) {
//      val curr = avoidanceMap(state)
//      avoidanceMap = avoidanceMap.updated(state, curr + avoidAtom)
//    } else {
//      avoidanceMap = avoidanceMap.updated(state,Set(avoidAtom))
//    }
//  }

  /*
  override def setIn(rule: NormalRule) = {
    super.setIn(rule)
    if (!extensional(rule.head)) {
      currentStateStatus = currentStateStatus.updated(rule.head,in)
      currentStateSupport = currentStateSupport.updated(rule.head,rule.body)
    }
  }

  override def setOut(a: Atom) = {
    super.setOut(a)
    if (!extensional(a)) {
      currentStateStatus = currentStateStatus.updated(a,out)
      currentStateSupport = currentStateSupport.updated(a,supp(a))
    }
  }


  def setUnknown(a: Atom) = {
    status(a) = unknown
    supp(a) = Set()
    suppRule(a) = None
  }
  */


  /*
  def partialStateSnapshot(): PartialState = {
    // ugly hacks around mutability problems - todo
    val partialStatus: Map[Atom, Status] = {
      val map1: scala.collection.Map[Atom, Status] = status filterKeys isStateAtom
      val map2 = scala.collection.mutable.Map[Atom, Status]()
      for ((k,v) <- map1) {
        map2 += k -> v
      }
      map2.toMap
    }
    val partialSupp: Map[Atom, scala.collection.immutable.Set[Atom]] = {
      val map1: scala.collection.Map[Atom, Set[Atom]] = supp filterKeys isStateAtom
      val map2 = scala.collection.mutable.Map[Atom, scala.collection.immutable.Set[Atom]]()
      for ((k,v) <- map1) {
        val set = v.toSet filter (!extensional(_))
        map2 += k -> set
      }
      map2.toMap
    }
    PartialState(partialStatus,partialSupp)
  }
  */

  def stateSnapshot(): Option[PartialState] = {

    // ugly hacks around mutability problems - todo
    val partialStatus: Map[Atom, Status] = {
      val map1: scala.collection.Map[Atom, Status] = status filterKeys isStateAtom
      val map2 = scala.collection.mutable.Map[Atom, Status]()
      for ((k,v) <- map1) {
        map2 += k -> v
      }
      map2.toMap
    }
    val partialSupp: Map[Atom, scala.collection.immutable.Set[Atom]] = {
      val map1: scala.collection.Map[Atom, Set[Atom]] = supp filterKeys isStateAtom
      val map2 = scala.collection.mutable.Map[Atom, scala.collection.immutable.Set[Atom]]()
      for ((k,v) <- map1) {
        val set = v.toSet filter (!extensional(_))
        map2 += k -> set
      }
      map2.toMap
    }

    Some(PartialState(partialStatus,partialSupp))

  }

  //skip facts! - for asp they are irrelevant, for tms they change based on time - no stable basis
  def isStateAtom(a: Atom): Boolean = (status(a) == in || status(a) == out) && !extensional(a)

  def selectNextAtom(): Unit = {

    state = stateSnapshot()

    val atoms = unknownAtoms filter (!extensional(_))

    if (atoms.isEmpty) return

    val atomsToAvoid= tabu.atomsToAvoid()

    selectedAtom = atoms find (!atomsToAvoid.contains(_))

    if (selectedAtom.isEmpty && prevState.isDefined) {
      tabu.avoid(prevState.get,prevSelectedAtom.get)
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

    /*
    val javaList = new java.util.ArrayList[Atom]()
    for (a <- atoms) {
      javaList.add(a)
    }
    if (shuffle) {
      java.util.Collections.shuffle(javaList)
    }

    val iterator: util.Iterator[Atom] = javaList.iterator()

    var elem: Atom = iterator.next()

    val atomsToAvoid = avoidanceMap.getOrElse(state.get,scala.collection.immutable.Set())

    while ((atomsToAvoid contains elem) && iterator.hasNext) {
      elem = iterator.next()
    }

    if (atomsToAvoid contains elem) {
      if (prevState.isDefined) {
        updateAvoidanceMap(prevState.get, prevSelectedAtom.get)
      }
      selectedAtom = None
    } else {
      selectedAtom = Some(elem)
    }

    */

  }

}