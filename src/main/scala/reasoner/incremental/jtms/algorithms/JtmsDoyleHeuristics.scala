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
class JtmsDoyleHeuristics(override val network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(), override val random: Random = new Random(),
                          val cfg: JtmsHeuristicsConfig = JtmsHeuristicsConfig(updateSuppRule = true, prevModel=false)) extends JtmsDoyle(network,random) {

  var prevModel = if (cfg.prevModel) { network.inAtoms } else { null }

  override def update(atoms: Set[Atom]): Unit = {
    if (cfg.prevModel) { prevModel = network.inAtoms }  else { null }
    super.update(atoms)
  }

  override def updateImplementation(atoms: Set[Atom]): Unit = {
    setUnknown(atoms)
    findStatus(atoms)
    if (cfg.prevModel) {
      chooseStatusPreviousModel(prevModel) //addition (explicit for profiling)
    }
    chooseStatus(atoms)
  }

  def setUnknown(atoms: Set[Atom]): Unit = {
    atoms foreach setUnknown
  }

  def findStatus(atoms: Set[Atom]): Unit = {
    atoms foreach findStatus
  }

  def chooseStatusPreviousModel(atoms: Set[Atom]): Unit = {
    atoms foreach chooseStatus
  }

  def chooseStatus(atoms: Set[Atom]): Unit = {
    atoms foreach chooseStatus
  }

  override def ruleAlreadyInHeuristic(rule: NormalRule): Unit = {
    if (!cfg.updateSuppRule) return
    if (network.valid(rule)) {
      val foundations = rule.body flatMap network.foundations
      if (!foundations.contains(rule.head)) {
        //difference to original; optimization for sliding time-based window (support always by latest)
        setIn(rule)
      }
    }
  }

}

case class JtmsHeuristicsConfig(updateSuppRule: Boolean, prevModel: Boolean)
