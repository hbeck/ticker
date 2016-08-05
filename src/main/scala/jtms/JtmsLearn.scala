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

  /*
  case class State(statusIn: scala.collection.immutable.Set[Atom], suppRules: scala.collection.immutable.Set[NormalRule]) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("State[\n").append("  in: ").append(statusIn).append("\n  supp. rules:  ").append(suppRules).append("]")
      sb.toString
    }
  }
  */


  var state: State = stateSnapshot()
  var selectedAtom: Option[Atom] = None

  var prevState: Option[State] = None
  var prevSelectedAtom: Option[Atom] = None

  override def updateGreedy(atoms: Predef.Set[Atom]) {
    atoms foreach setUnknown
    if (needReset) {
      atomsNeedingSupp() foreach setUnknown
    }
    while (hasUnknown) {
      unknownAtoms foreach findStatus
      state = stateSnapshot()
      val atom = selectNextAtom
      if (atom.isDefined) {
        selectedAtom = atom
        chooseStatusGreedy(atom.get)
        prevState = Some(state)
        prevSelectedAtom = selectedAtom
      } else if (hasUnknown) {
        //invalidateModel()
        throw new IncrementalUpdateFailureException()
      }
    }
    //reset for next update iteration
    prevState = None
    prevSelectedAtom = None
  }

  def needReset(): Boolean = {
    state = stateSnapshot()
    //val avoidSet = avoidanceMap.getOrElse(state,Set[Atom]())
    //(unknownAtoms diff avoidSet).isEmpty
    val atom = selectNextAtom()
    atom.isEmpty
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
      updateAvoidanceMap(this.state,selectedAtom.get)
    }

    super.invalidateModel()
  }

  def updateAvoidanceMap(state: State, avoidAtom: Atom): Unit = {
    //val updatedRules = state.rules ++ justifications(avoidAtom)
    //val state = State(state.status,state.support,updatedRules)
    if (avoidanceMap contains state) {
      val curr = avoidanceMap(state)
      avoidanceMap(state) = curr + avoidAtom
    } else {
      avoidanceMap(state) = scala.collection.immutable.Set(avoidAtom)
    }
  }

  def stateSnapshot(): State = {

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

    State(partialStatus,partialSupp,rules.toSet)


    //only supporting rules' atoms
    /*
    val atoms = inAtoms
    val supportingRules = Predef.Set[NormalRule]() ++ (atoms map suppRule map (_.get))
    State(atoms, supportingRules)
    */

    /*
    val rules = (outAtoms flatMap justifications) ++ (inAtoms map suppRule map (_.get))
    State(partialStatus,partialSupp,rules.toSet)
    */

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

    val javaList = new java.util.ArrayList[Atom]()
    for (a <- atoms) {
      javaList.add(a)
    }
    if (shuffle) {
      java.util.Collections.shuffle(javaList)
    }

    val iterator: util.Iterator[Atom] = javaList.iterator()

    var elem: Atom = iterator.next()

    while (avoidAtom(state,elem) && iterator.hasNext) {
      elem = iterator.next()
    }

    if (avoidAtom(state,elem)) {
      selectedAtom = None
      if (prevState.isDefined) {
        updateAvoidanceMap(prevState.get, prevSelectedAtom.get)
        //val rules = prevState.get.rules ++ state.rules ++ justifications(elem)
        val updatedPrevState = State(prevState.get.status, prevState.get.support, rules.toSet)
        updateAvoidanceMap(updatedPrevState,prevSelectedAtom.get)
      }
      return None
    }

    Some(elem)

  }

  def avoidAtom(state: State, atom: Atom): Boolean = {
    //val updatedRules = partialState.rules ++ justifications(atom)
    //val updatedState = State(partialState.status,partialState.support,rules.toSet)
    //val atomsToAvoid = avoidanceMap.getOrElse(updatedState,scala.collection.immutable.Set())
    val atomsToAvoid = avoidanceMap.getOrElse(state,scala.collection.immutable.Set())
    atomsToAvoid contains atom
  }

  //history
  val avoidanceMap = new scala.collection.mutable.HashMap[State,scala.collection.immutable.Set[Atom]]()

}