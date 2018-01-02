package reasoner.config

import reasoner.Reasoner
import reasoner.asp.tms.policies.{ImmediatelyAddRemovePolicy, LazyRemovePolicy}
import reasoner.config.EvaluationModifier.EvaluationModifier
import reasoner.config.ReasonerChoice.ReasonerChoice
import reasoner.incremental.jtms.algorithms.Jtms
import reasoner.incremental.jtms.networks.TruthMaintenanceNetwork

import scala.util.Random

/**
  * Created by FM on 29.08.16.
  */
object ReasonerChoice extends Enumeration {
  type ReasonerChoice = Value
  val Incremental, Clingo = Value
}

object EvaluationModifier extends Enumeration {
  type EvaluationModifier = Value
  val LazyRemove, Incremental, Push, Pull = Value
}

case class ArgumentBasedConfiguration(config: Configuration) {

  def build(evaluationType: ReasonerChoice, evaluationModifier: EvaluationModifier) = buildEngine(evaluationType, evaluationModifier)

  def buildEngine(reasonerChoice: ReasonerChoice,
                  evaluationModifier: EvaluationModifier,
                  network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(),
                  random: Random = new Random(1)): Option[Reasoner] = {

    if (reasonerChoice == ReasonerChoice.Incremental) {
      evaluationModifier match {
        case EvaluationModifier.LazyRemove => return Some(jtmsLazyRemove(config, network, random))
        case EvaluationModifier.Incremental => return Some(jtmsIncremental(config, network, random))
        case _ => None
      }
    } else if (reasonerChoice == ReasonerChoice.Clingo) {
      if (evaluationModifier == EvaluationModifier.Push) {
        return Some(clingoPush(config))
      } else if (evaluationModifier == EvaluationModifier.Pull) {
        return Some(clingoPull(config))
      }
    }

    None
  }

  //TODO hb does it make sense?
  def jtmsLazyRemove(config: Configuration, network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(), random: Random = new Random(1)) = {
    val jtms = Jtms(network, random)
    jtms.recordStatusSeq = false
    jtms.recordChoiceSeq = false

    config.configure().withIncremental().withPolicy(LazyRemovePolicy(jtms)).seal()
  }

  def jtmsIncremental(config: Configuration, network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(), random: Random = new Random(1)) = {
    val jtms = Jtms(network, random)
    jtms.recordStatusSeq = false
    jtms.recordChoiceSeq = false

    //TODO hb ".withIncremental" should not be needed
    config.configure().withIncremental().withPolicy(ImmediatelyAddRemovePolicy(jtms)).use().seal()
  }


  //TODO hb "use.use.."?
  def clingoPush(config: Configuration) = {
    config.configure().withClingo().withDefaultEvaluationMode().usePush().seal()
  }

  //TODO hb "use.use.."?
  def clingoPull(config: Configuration) = {
    config.configure().withClingo().withDefaultEvaluationMode().usePull().seal()
  }
}