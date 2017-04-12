package jtms.algorithms

import core.Atom
import jtms.TruthMaintenanceNetwork

import scala.util.Random

/**
  * Created by hb on 12.04.17. Doyle with additional Heuristics
  */
class JtmsDoyleHeuristics(override val network: TruthMaintenanceNetwork, override val random: Random = new Random()) extends JtmsDoyle(network,random) {

  var prevModel = network.inAtoms

  override def update(atoms: Set[Atom]): Unit = {
    prevModel = network.inAtoms
    super.update(atoms)
  }

  override def updateImplementation(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown
    atoms foreach findStatus

    //addition:
    prevModel foreach chooseStatus

    atoms foreach chooseStatus
  }

}
