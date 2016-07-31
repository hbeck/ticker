package jtms

import java.util

import core._
import core.asp.NormalProgram

import scala.collection.mutable.Set
import scala.collection.{Map, mutable}
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

  recordStatusSeq = true
  recordChoiceSeq = true

  case class State(status: Map[Atom, Status], support: Map[Atom, scala.collection.immutable.Set[Atom]]) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("State[\n").append("  status:  ").append(status).append("\n  support: ").append(support).append("]")
      sb.toString
    }
  }

  var state: State = stateSnapshot()
  var selectedAtom: Option[Atom] = None
  var previousState: Option[State] = None
  var previousAtom: Option[Atom] = None

  override def updateGreedy(atoms: Set[Atom]) {
    atoms foreach setUnknown
    while (hasUnknown) {
      unknownAtoms foreach findStatus
      val atom = selectNextAtom
      if (atom.isDefined) {
        selectedAtom = atom
        chooseStatusGreedy(atom.get)
      } else if (hasUnknown && previousState.isDefined) { //avoid previous choice
        state = previousState.get
        selectedAtom = previousAtom
        invalidateModel()
      }
      previousState = Some(state)
      previousAtom = selectedAtom
    }
  }

  def shuffleStatusSeq(): Seq[Atom] = {
    val javaList = new java.util.ArrayList[Atom]
    for (a <- allAtoms()) {
      javaList.add(a)
    }
    java.util.Collections.shuffle(javaList)
    var nextSeq = Seq[Atom]()
    val iterator: util.Iterator[Atom] = javaList.iterator()
    while (iterator.hasNext()) {
      nextSeq = nextSeq :+ iterator.next()
    }
    println("next seq: "+nextSeq)
    nextSeq
  }

  //note that in this case, invalidateModel is not supposed to be called from outside!
  override def invalidateModel(): Unit = {

    if (selectedAtom.isDefined) {
      if (avoidanceMap contains state) {
        avoidanceMap(state) + selectedAtom.get
      } else {
        avoidanceMap(state) = Set(selectedAtom.get)
      }
    }

    super.invalidateModel()
  }

  def stateSnapshot(): State = {

    val inOutAtoms = inAtoms union outAtoms
    // ugly hacks around mutability problems - todo
    val partialStatus: Map[Atom, Status] = {
      val map1: Map[Atom, Status] = status filterKeys (inOutAtoms contains _)
      val map2 = scala.collection.mutable.Map[Atom, Status]()
      for ((k,v) <- map1) {
        map2 += k -> v
      }
      map2.toMap
    }
    val partialSupp: Map[Atom, scala.collection.immutable.Set[Atom]] = {
      val map1: Map[Atom, mutable.Set[Atom]] = supp filterKeys (inOutAtoms contains _)
      val map2 = scala.collection.mutable.Map[Atom, scala.collection.immutable.Set[Atom]]()
      for ((k,v) <- map1) {
        val set = v.toSet
        map2 += k -> set
      }
      map2.toMap
    }

    State(partialStatus,partialSupp)

  }

  def selectNextAtom(): Option[Atom] = {

    val atoms = unknownAtoms

    if (atoms.isEmpty) return None
    //if (atoms.size == 1) return Some(atoms.head)

    if (doForceChoiceOrder) {
      val maybeAtom: Option[Atom] = forcedChoiceSeq find (status(_) == unknown)
      if (maybeAtom.isDefined) {
        return maybeAtom
      }
    }

    state = stateSnapshot()

    val javaList = new java.util.ArrayList[Atom]()
    for (a <- atoms) {
      javaList.add(a)
    }
    if (shuffle) {
      java.util.Collections.shuffle(javaList)
    }

    val iterator: util.Iterator[Atom] = javaList.iterator()

    var elem: Atom = iterator.next()

    val atomsToAvoid = avoidanceMap.getOrElse(state,Set())

    while ((atomsToAvoid contains elem) && iterator.hasNext) {
      elem = iterator.next()
    }

    //in general, avoided atom but become okay - todo

    if (atomsToAvoid contains elem) {
      selectedAtom = None
      return None
    }

    Some(elem)

  }

  //history
  var avoidanceMap = new scala.collection.mutable.HashMap[State,Set[Atom]]()

}