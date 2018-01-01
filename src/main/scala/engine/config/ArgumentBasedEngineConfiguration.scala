package engine.config

import engine.Engine
import engine.asp.tms.policies.{ImmediatelyAddRemovePolicy, LazyRemovePolicy}
import engine.config.EvaluationModifier.EvaluationModifier
import engine.config.Reasoner.Reasoner
import jtms.algorithms.Jtms
import jtms.networks.TruthMaintenanceNetwork

import scala.util.Random

/**
  * Created by FM on 29.08.16.
  */
object Reasoner extends Enumeration {
  type Reasoner = Value
  val Incremental, Clingo = Value
}

object EvaluationModifier extends Enumeration {
  type EvaluationModifier = Value
  val LazyRemove, Incremental, Push, Pull = Value
}

case class ArgumentBasedEngineConfiguration(config: EngineConfiguration) {

  def build(evaluationType: Reasoner, evaluationModifier: EvaluationModifier) = buildEngine(evaluationType, evaluationModifier)

  def buildEngine(evaluationType: Reasoner,
                  evaluationModifier: EvaluationModifier,
                  network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(),
                  random: Random = new Random(1)): Option[Engine] = {

    if (evaluationType == Reasoner.Incremental) {
      evaluationModifier match {
        case EvaluationModifier.LazyRemove => return Some(jtmsLazyRemove(config, network, random))
        case EvaluationModifier.Incremental => return Some(jtmsIncremental(config, network, random))
        case _ => None
      }
    } else if (evaluationType == Reasoner.Clingo) {
      if (evaluationModifier == EvaluationModifier.Push) {
        return Some(clingoPush(config))
      } else if (evaluationModifier == EvaluationModifier.Pull) {
        return Some(clingoPull(config))
      }
    }

    None
  }

  //TODO hb does it make sense?
  def jtmsLazyRemove(config: EngineConfiguration, network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(), random: Random = new Random(1)) = {
    val jtms = Jtms(network, random)
    jtms.recordStatusSeq = false
    jtms.recordChoiceSeq = false

    config.configure().withJtms().withPolicy(LazyRemovePolicy(jtms)).seal()
  }

  def jtmsIncremental(config: EngineConfiguration, network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(), random: Random = new Random(1)) = {
    val jtms = Jtms(network, random)
    jtms.recordStatusSeq = false
    jtms.recordChoiceSeq = false

    //TODO hb ".withIncremental" should not be needed
    config.configure().withJtms().withPolicy(ImmediatelyAddRemovePolicy(jtms)).withIncremental().seal()
  }


  //TODO hb "use.use.."?
  def clingoPush(config: EngineConfiguration) = {
    config.configure().withClingo().use().usePush().seal()
  }

  //TODO hb "use.use.."?
  def clingoPull(config: EngineConfiguration) = {
    config.configure().withClingo().use().usePull().seal()
  }
}