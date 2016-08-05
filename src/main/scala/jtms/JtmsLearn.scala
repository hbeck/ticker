package jtms

import java.util

import core._
import core.asp.{NormalProgram, NormalRule}

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

  var state: Option[State] = None
  var selectedAtom: Option[Atom] = None

  var prevState: Option[State] = None
  var prevSelectedAtom: Option[Atom] = None

  override def updateGreedy(atoms: Predef.Set[Atom]) {
    atoms foreach setUnknown
    //test avoidance map before determining further consequences
    selectNextAtom()
    if (selectedAtom.isEmpty) {
      atomsNeedingSupp() foreach setUnknown
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

  //note that in this case, invalidateModel is not supposed to be called from outside!
  override def invalidateModel(): Unit = {

    if (selectedAtom.isDefined) {
      updateAvoidanceMap(this.state.get,selectedAtom.get)
    }

    super.invalidateModel()
  }

  def updateAvoidanceMap(state: State, avoidAtom: Atom): Unit = {
    if (avoidanceMap contains state) {
      val curr = avoidanceMap(state)
      avoidanceMap(state) = curr + avoidAtom
    } else {
      avoidanceMap(state) = scala.collection.immutable.Set(avoidAtom)
    }
  }

  def stateSnapshot(): Option[State] = {

    val atoms = inAtoms union outAtoms
    // ugly hacks around mutability problems - todo
    val partialStatus: Map[Atom, Status] = {
      val map1: scala.collection.Map[Atom, Status] = status filterKeys (atoms contains _)
      val map2 = scala.collection.mutable.Map[Atom, Status]()
      for ((k,v) <- map1) {
        map2 += k -> v
      }
      map2.toMap
    }
    val partialSupp: Map[Atom, scala.collection.immutable.Set[Atom]] = {
      val map1: scala.collection.Map[Atom, scala.collection.mutable.Set[Atom]] = supp filterKeys (atoms contains _)
      val map2 = scala.collection.mutable.Map[Atom, scala.collection.immutable.Set[Atom]]()
      for ((k,v) <- map1) {
        val set = v.toSet
        map2 += k -> set
      }
      map2.toMap
    }

    Some(State(partialStatus,partialSupp,rules.toSet))

  }

  def selectNextAtom(): Unit = {

    state = stateSnapshot()

    val atoms = unknownAtoms

    if (atoms.isEmpty) return None

    if (doForceChoiceOrder) {
      val maybeAtom: Option[Atom] = forcedChoiceSeq find (status(_) == unknown)
      if (maybeAtom.isDefined) {
        return maybeAtom
      }
    }

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
        val updatedPrevState = State(prevState.get.status, prevState.get.support, rules.toSet)
        updateAvoidanceMap(updatedPrevState,prevSelectedAtom.get)
      }
      selectedAtom = None
    } else {
      selectedAtom = Some(elem)
    }

  }

  //history
  val avoidanceMap = new scala.collection.mutable.HashMap[State,scala.collection.immutable.Set[Atom]]()

}