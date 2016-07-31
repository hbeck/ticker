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

  case class State(status: Map[Atom, Status], support: Map[Atom, mutable.Set[Atom]]) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("State[\n").append("  status:  ").append(status).append("\n  support: ").append(support).append("]")
      sb.toString
    }
  }

  var state: State = stateSnapshot()
  var selectedAtom: Option[Atom] = None
  var atomShouldBeAvoided: Boolean = false

  override def updateGreedy(atoms: Set[Atom]) {
    atoms foreach setUnknown
    while (hasUnknown) {
      unknownAtoms foreach findStatus
      state = stateSnapshot()
      val atom = selectNextAtom
      if (atom.isDefined) {
        selectedAtom = atom
        chooseStatusGreedy(atom.get)
      }
    }
  }

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
    val partialStatus: Map[Atom, Status] = status filterKeys (inOutAtoms contains _)
    val partialSupp: Map[Atom, mutable.Set[Atom]] = supp filterKeys (inOutAtoms contains _)

    State(partialStatus,partialSupp)

  }

  def selectNextAtom(): Option[Atom] = {

    val atoms = unknownAtoms

    if (atoms.isEmpty) return None
    if (atoms.size == 1) return Some(atoms.head)

    if (doForceChoiceOrder) {
      val maybeAtom: Option[Atom] = forcedChoiceSeq find (status(_) == unknown)
      if (maybeAtom.isDefined) {
        return maybeAtom
      }
    }

    //val list = List[Atom]() ++ atoms
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

    //in general, avoided atom but become okay

    Some(elem)

  }

  //history
  var avoidanceMap = new scala.collection.mutable.HashMap[State,Set[Atom]]()

}