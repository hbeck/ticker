package reasoner.config

import reasoner.Reasoner
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
  val incremental, clingo = Value
}

object EvaluationModifier extends Enumeration {
  type EvaluationModifier = Value
  val Push, Pull = Value
}

case class ArgumentBasedConfiguration(config: Configuration) {

  def build(evaluationType: ReasonerChoice, evaluationModifier: EvaluationModifier) = buildEngine(evaluationType, evaluationModifier)

  def buildEngine(reasonerChoice: ReasonerChoice,
                  evaluationModifier: EvaluationModifier,
                  network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(),
                  random: Random = new Random(1)): Option[Reasoner] = {

    if (reasonerChoice == ReasonerChoice.incremental) {
      val jtms = Jtms(network, random)
      jtms.recordStatusSeq = false
      jtms.recordChoiceSeq = false
      val reasoner = config.configure().withIncremental().withJtms(jtms).use().seal()
      return Some(reasoner)
    } else if (reasonerChoice == ReasonerChoice.clingo) {
      if (evaluationModifier == EvaluationModifier.Push) {
        return Some(clingoPush(config))
      } else if (evaluationModifier == EvaluationModifier.Pull) {
        return Some(clingoPull(config))
      }
    }

    None
  }

  def clingoPush(config: Configuration) = {
    config.configure().withClingo().withDefaultEvaluationMode().usePush().seal()
  }

  def clingoPull(config: Configuration) = {
    config.configure().withClingo().withDefaultEvaluationMode().usePull().seal()
  }
}