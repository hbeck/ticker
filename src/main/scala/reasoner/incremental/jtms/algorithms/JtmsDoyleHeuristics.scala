package reasoner.incremental.jtms.algorithms

import core.Atom
import core.asp.{NormalProgram, NormalRule}
import reasoner.incremental.jtms.networks.TruthMaintenanceNetwork

import scala.util.Random

object JtmsDoyleHeuristics {

  def apply(P: NormalProgram): JtmsDoyleHeuristics = {
    val jtms = new JtmsDoyleHeuristics()
    P.rules foreach jtms.add
    jtms
  }

  def apply(random: Random = new Random()): JtmsDoyleHeuristics = new JtmsDoyleHeuristics(TruthMaintenanceNetwork(),random)

}

/**
  * Created by hb on 12.04.17. Doyle with additional Heuristics
  */
class JtmsDoyleHeuristics(override val network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(), override val random: Random = new Random()) extends JtmsDoyle(network,random) {

  var prevModel = network.inAtoms

  override def update(atoms: Set[Atom]): Unit = {
    prevModel = network.inAtoms
    super.update(atoms)
  }

  override def updateImplementation(atoms: Set[Atom]): Unit = {

    reset(atoms)
    find(atoms)

    //addition:
    choosePrev(prevModel)

    choose(atoms)
  }

  def reset(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown
  }

  def find(atoms: Set[Atom]): Unit = {
    atoms foreach findStatus
  }

  def choosePrev(atoms: Set[Atom]): Unit = {
    atoms foreach chooseStatus
  }

  def choose(atoms: Set[Atom]): Unit = {
    atoms foreach chooseStatus
  }

  override def ruleAlreadyInHeuristic(rule: NormalRule): Unit = {
    if (network.valid(rule)) {
      val foundations = rule.body flatMap network.foundations
      if (!foundations.contains(rule.head)) {
        //difference to original; optimization for sliding time-based window (support always by latest)
        setIn(rule)
      }
    }
  }

}
