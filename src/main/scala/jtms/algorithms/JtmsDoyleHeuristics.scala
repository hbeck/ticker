package jtms.algorithms

import core.Atom
import core.asp.NormalRule
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

  override def ruleAlreadyInHeuristic(rule: NormalRule): Unit = {
    if (network.valid(rule)) {
      // TODO: this could create self-support!
      // Scenario:
      // dz is in, support is: dz :- some, w_te_2_d_z.
      // new rule is added: dz :- dz_at(18).
      // is already valid, creates self-support :-/

      // TODO: do a benchmark here
      val foundations = rule.body flatMap network.foundations
      if (!foundations.contains(rule.head)) {
        //difference to original; optimization for sliding time-based window (support always by latest)
        setIn(rule)
      }
    }
  }

}
