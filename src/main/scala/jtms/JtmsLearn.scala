package jtms

import core._
import core.asp.{NormalProgram, NormalRule}

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

  case class State(status: Predef.Map[Atom, Status], support: Predef.Map[Atom, scala.collection.immutable.Set[Atom]], rules: scala.collection.immutable.Set[NormalRule]) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("State[\n").append("  rules:  ").append(rules).append("\n")
          .append("  status:  ").append(status).append("\n  support: ").append(support).append("]")
      sb.toString
    }
  }

  override def updateGreedy(atoms: Predef.Set[Atom]) {
    atoms foreach setUnknown
    //test avoidance map before determining further consequences
    selectNextAtom()
    if (selectedAtom.isEmpty) {
      atomsNeedingSupp() foreach setUnknown
    } else {
      prevState = state
      prevSelectedAtom = selectedAtom
    }
    while (hasUnknown) {
      unknownAtoms foreach findStatus
      selectNextAtom()
      selectedAtom match {
        case Some(atom) => {
          chooseStatusGreedy(atom)
          prevState = state
          prevSelectedAtom = selectedAtom
        }
        case None => if (hasUnknown) throw new IncrementalUpdateFailureException()
      }
    }
    //reset for next update iteration
    prevState = None
    prevSelectedAtom = None
  }

  var state: Option[State] = None
  var selectedAtom: Option[Atom] = None

  var prevState: Option[State] = None
  var prevSelectedAtom: Option[Atom] = None

  var currentRuleHash: Int = -1
  var currentStateRules: Set[NormalRule] = Predef.Set()

  override def register(rule: NormalRule): Boolean = {
    val newRule = super.register(rule)
    if (!newRule) {
      return false
    } else if (dataIndependentRule(rule)) {
      currentStateRules = currentStateRules + rule
      currentRuleHash = currentStateRules.hashCode
    }
    true
  }

  override def unregister(rule: NormalRule): Boolean = {
    val ruleExisted = super.unregister(rule)
    if (!ruleExisted) {
      return false
    } else if(dataIndependentRule(rule)) {
      currentStateRules = currentStateRules - rule
      currentRuleHash = currentStateRules.hashCode
    }
    true
  }



  //note that in this case, invalidateModel is not supposed to be called from outside!
  override def invalidateModel(): Unit = {

    if (selectedAtom.isDefined) {
      updateAvoidanceMap(this.state.get,selectedAtom.get)
    }

    super.invalidateModel()
  }

  def updateAvoidanceMap(state: State, avoidAtom: Atom): Unit = {
//    println("\nupdateAvoidanceMap:")
//    println(state)
//    println("avoid atom: "+avoidAtom+"\n")
    if (avoidanceMap contains state) {
      val curr = avoidanceMap(state)
      avoidanceMap(state) = curr + avoidAtom
    } else {
      avoidanceMap(state) = scala.collection.immutable.Set(avoidAtom)
    }
  }

  var currentStateStatus: Map[Atom,Status] = new HashMap[Atom,Status]()
  var currentStateSupport: Map[Atom,Predef.Set[Atom]] = new HashMap[Atom,Predef.Set[Atom]]()

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

  def stateSnapshot(): Option[State] = {

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

    Some(State(partialStatus,partialSupp,dataIndependentRules.toSet))

  }

  //skip facts! - for asp they are irrelevant, for tms they change based on time - no stable basis
  def isStateAtom(a: Atom): Boolean = (status(a) == in || status(a) == out) && !extensional(a)

  def selectNextAtom(): Unit = {

    //TODO assume state is maintained !
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

    val avoid = avoidanceMap.getOrElse(state.get,scala.collection.immutable.Set())

    selectedAtom = atoms find (!avoid.contains(_))

    if (selectedAtom.isEmpty && prevState.isDefined) {
      updateAvoidanceMap(prevState.get, prevSelectedAtom.get)
    }

  }

  //history
  val avoidanceMap = new scala.collection.mutable.HashMap[State,scala.collection.immutable.Set[Atom]]()

}