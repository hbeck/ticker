package jtms.algorithms

import jtms.TruthMaintenanceNetwork
import core.{Atom, PinnedAtom}

import scala.util.Random

/**
  * Created by hb on 12.04.17.
  */
class JtmsGreedyHeuristics(network: TruthMaintenanceNetwork, random: Random = new Random()) extends JtmsGreedy(network, random) {

  var prevModel = network.inAtoms

  override def update(atoms: Set[Atom]): Unit = {
    prevModel = network.inAtoms
    super.update(atoms)
  }

  override def updateImplementation(atoms: Set[Atom]) {
    atoms foreach setUnknown
    var lastAtom: Option[Atom] = None
    while (network.hasUnknown) {
      network.unknownAtoms foreach findStatus
      val atom = getOptUnknownOtherThan(lastAtom) //ensure that the same atom is not tried consecutively
      if (atom.isDefined) {
        chooseStatusGreedy(atom.get)
      }
      lastAtom = atom
    }
  }

  override def getOptUnknownOtherThan(avoid: Option[Atom]): Option[Atom] = {

    if (!prevModel.isEmpty) {
      val a = prevModel.head
      prevModel = prevModel.tail
      return Some(a)
    }

    val atomSet = network.unknownAtoms filter (!_.isInstanceOf[PinnedAtom])

    if (atomSet.isEmpty) return None
    if (atomSet.size == 1) return Some(atomSet.head)

    val atoms = if (shuffle && atomSet.size > 1) (random.shuffle(atomSet.toSeq)) else atomSet

    Some(atoms.head)
  }

}
