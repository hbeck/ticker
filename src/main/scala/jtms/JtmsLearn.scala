package jtms

import java.util

import core._
import core.asp.{NormalProgram, NormalRule}

import scala.collection.mutable.Set
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

//  case class State(status: Map[Atom, Status], support: Map[Atom, scala.collection.immutable.Set[Atom]], rules: scala.collection.immutable.Set[NormalRule]) {
//    override def toString: String = {
//      val sb = new StringBuilder
//      sb.append("State[\n").append("  rules:  ").append(rules).append("\n")
//          .append("  status:  ").append(status).append("\n  support: ").append(support).append("]")
//      sb.toString
//    }
//  }

  case class State(statusIn: scala.collection.immutable.Set[Atom], suppRules: scala.collection.immutable.Set[NormalRule]) {
    override def toString: String = {
      val sb = new StringBuilder
      sb.append("State[\n").append("  in: ").append(statusIn).append("\n  supp. rules:  ").append(suppRules).append("]")
      sb.toString
    }
  }

  var state: State = stateSnapshot()
  var selectedAtom: Option[Atom] = None

  override def updateGreedy(atoms: Set[Atom]) {
    atoms foreach setUnknown
    while (hasUnknown) {
      unknownAtoms foreach findStatus
      val atom = selectNextAtom
      if (atom.isDefined) {
        selectedAtom = atom
        chooseStatusGreedy(atom.get)
      } else if (hasUnknown) {
        invalidateModel()
      }
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
        val curr = avoidanceMap(state)
        avoidanceMap(state) = curr + selectedAtom.get
      } else {
        avoidanceMap(state) = scala.collection.immutable.Set(selectedAtom.get)
      }
    }

    super.invalidateModel()
  }

  def stateSnapshot(): State = {

    /*
    val atoms = inAtoms union outAtoms
    // ugly hacks around mutability problems - todo
    val partialStatus: Map[Atom, Status] = {
      val map1: Map[Atom, Status] = status filterKeys (atoms contains _)
      val map2 = scala.collection.mutable.Map[Atom, Status]()
      for ((k,v) <- map1) {
        map2 += k -> v
      }
      map2.toMap
    }
    val partialSupp: Map[Atom, scala.collection.immutable.Set[Atom]] = {
      val map1: Map[Atom, mutable.Set[Atom]] = supp filterKeys (atoms contains _)
      val map2 = scala.collection.mutable.Map[Atom, scala.collection.immutable.Set[Atom]]()
      for ((k,v) <- map1) {
        val set = v.toSet
        map2 += k -> set
      }
      map2.toMap
    }

    State(partialStatus,partialSupp,rules.toSet)
    */

    val atoms = inAtoms
    val supportingRules = Predef.Set[NormalRule]() ++ (atoms map suppRule map (_.get))
    State(atoms, supportingRules)

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

    val atomsToAvoid = avoidanceMap.getOrElse(state,scala.collection.immutable.Set())

    while ((atomsToAvoid contains elem) && iterator.hasNext) {
      elem = iterator.next()
    }

    if (atomsToAvoid contains elem) {
      selectedAtom = None
      return None
    }

    Some(elem)

  }

  //history
  val avoidanceMap = new scala.collection.mutable.HashMap[State,scala.collection.immutable.Set[Atom]]()

}