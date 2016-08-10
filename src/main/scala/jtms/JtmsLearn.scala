package jtms

import core._
import core.asp.NormalProgram

import scala.collection.immutable.HashMap
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

  var avoidanceMap = new HashMap[PartialState,Set[Atom]]()

  //

  /*

  var tabuMap: Map[Int, Map[PartialState, Set[Atom]]] = new HashMap[Int,Map[PartialState,Set[Atom]]]

  case class PartialState(var status: Map[Atom, Status], var support: Map[Atom, Set[Atom]])

  var prevRuleHash: Int = -1
  var prevStateRules: Set[NormalRule] = Set()
  var prevPartialStateMap: Map[PartialState,Set[Atom]] = new HashMap[PartialState,Set[Atom]]
  var prevPartialState: PartialState = new PartialState(new HashMap[Atom,Status](), new HashMap[Atom,Set[Atom]])

  var currentRuleHash: Int = -1
  var currentStateRules: Set[NormalRule] = Set()
  var currentPartialStateMap: Map[PartialState,Set[Atom]] = new HashMap[PartialState,Set[Atom]]
  var partialState: PartialState = new PartialState(new HashMap[Atom,Status](), new HashMap[Atom,Set[Atom]])

  def ruleUpdateSwitchPartialState(): Unit = {
    currentRuleHash = currentStateRules.hashCode
    if (tabuMap contains currentRuleHash) {
      currentPartialStateMap = tabuMap(currentRuleHash)
    } else {
      currentPartialStateMap = new HashMap[PartialState,Set[Atom]] //new PartialState(new HashMap[Atom,Status](), new HashMap[Atom,Set[Atom]])
      tabuMap = tabuMap.updated(currentRuleHash,currentPartialStateMap)
    }
  }

  def updateTabuMap(partialState: PartialState, avoidAtom: Atom): Unit = {
    val set = currentPartialStateMap.getOrElse(partialState,Set()) + avoidAtom
    currentPartialStateMap = currentPartialStateMap.updated(partialState,set)
    tabuMap = tabuMap.updated(currentRuleHash,currentPartialStateMap)
  }
  */

  /*
  override def register(rule: NormalRule): Boolean = {
    val newRule = super.register(rule)
    if (!newRule) {
      return false
    } else if (dataIndependentRule(rule)) {
      currentStateRules = currentStateRules + rule
      ruleUpdateSwitchPartialState()
    }
    true
  }

  override def unregister(rule: NormalRule): Boolean = {
    val ruleExisted = super.unregister(rule)
    if (!ruleExisted) {
      return false
    } else if (dataIndependentRule(rule)) {
      currentStateRules = currentStateRules - rule
      ruleUpdateSwitchPartialState()
    }
    true
  }
  */

  //note that in this case, invalidateModel is not supposed to be called from outside!
  override def invalidateModel(): Unit = {

    if (selectedAtom.isDefined) {
      updateAvoidanceMap(this.state.get,selectedAtom.get)
    }

    super.invalidateModel()
  }

  def updateAvoidanceMap(state: PartialState, avoidAtom: Atom): Unit = {
//    println("\nupdateAvoidanceMap:")
//    println(state)
//    println("avoid atom: "+avoidAtom+"\n")
    if (avoidanceMap contains state) {
      val curr = avoidanceMap(state)
      avoidanceMap = avoidanceMap.updated(state, curr + avoidAtom)
    } else {
      avoidanceMap = avoidanceMap.updated(state,Set(avoidAtom))
    }
  }

  
  /*

  var currentStateStatus: Map[Atom,Status] = new HashMap[Atom,Status]()
  var currentStateSupport: Map[Atom,Set[Atom]] = new HashMap[Atom,Set[Atom]]()
  
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

    val atomsToAvoid = avoidanceMap.getOrElse(state.get,scala.collection.immutable.Set())

    selectedAtom = atoms find (!atomsToAvoid.contains(_))

    if (selectedAtom.isEmpty && prevState.isDefined) {
      updateAvoidanceMap(prevState.get,prevSelectedAtom.get)
    }

  }

}