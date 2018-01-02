package reasoner.config

import clingo.{ClingoConversion, ClingoProgramWithLars}
import core.Atom
import core.lars.{ClockTime, LarsProgram}
import reasoner.asp._
import reasoner.asp.oneshot._
import reasoner.asp.tms.policies.{ImmediatelyAddRemovePolicy, JtmsPolicy}
import reasoner.config.EvaluationModifier.EvaluationModifier
import reasoner.config.ReasonerChoice.ReasonerChoice
import reasoner.{Reasoner, ReasonerWithFilter, ResultFilter}
import jtms.algorithms.Jtms
import jtms.networks.OptimizedNetwork
import reasoner.incremental.{IncrementalReasoner, IncrementalRuleMaker}

import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by FM on 14.05.16.
  */
object BuildReasoner {
  def withProgram(program: LarsProgram) = Configuration(program)
}

case class Configuration(larsProgram: LarsProgram, clockTime: ClockTime = 1 second) {

  def withReasoning(reasonerChoice: ReasonerChoice, evaluationModifier: EvaluationModifier) = ArgumentBasedConfiguration(this).build(reasonerChoice, evaluationModifier)

  def configure() = ReasonerConfiguration(larsProgram, clockTime)

  def withClockTime(clockTime: ClockTime) = Configuration(larsProgram, clockTime)
}

case class ReasonerConfiguration(program: LarsProgram, clockTime: ClockTime) {

  private lazy val aspMapped = PlainLarsToAspMapper(clockTime)(program)

  def withClingo() = EvaluationModeConfiguration(ClingoConversion.fromLars(aspMapped))

  def withJtms(): JtmsConfiguration = JtmsConfiguration(aspMapped)

}

case class JtmsConfiguration(larsProgramEncoding: LarsProgramEncoding, policy: JtmsPolicy = ImmediatelyAddRemovePolicy()) {

  def withRandom(random: Random) = JtmsConfiguration(larsProgramEncoding, ImmediatelyAddRemovePolicy(Jtms(new OptimizedNetwork(), random)))

  def useJtms(jtms: Jtms) = JtmsConfiguration(larsProgramEncoding, ImmediatelyAddRemovePolicy(jtms))

  def withPolicy(jtmsPolicy: JtmsPolicy) = JtmsConfiguration(larsProgramEncoding, jtmsPolicy)

  def withIncremental() = PreparedReasonerConfiguration(
    IncrementalReasoner(IncrementalRuleMaker(larsProgramEncoding), policy),
    larsProgramEncoding.intensionalAtoms ++ larsProgramEncoding.signals
  )

}

object JtmsConfiguration {
  implicit def toEvaluationModeConfig(config: JtmsConfiguration): PreparedReasonerConfiguration =
    PreparedReasonerConfiguration(
      IncrementalReasoner(IncrementalRuleMaker(config.larsProgramEncoding), config.policy),
      config.larsProgramEncoding.intensionalAtoms ++ config.larsProgramEncoding.signals
    )
}

case class EvaluationModeConfiguration(clingoProgram: ClingoProgramWithLars) {

  def use(evaluationMode: EvaluationMode = Direct) = {
    val aspEvaluation = buildEvaluationMode(OneShotClingoEvaluation(clingoProgram, StreamingClingoInterpreter(clingoProgram)), evaluationMode)
    EvaluationStrategyConfiguration(aspEvaluation)
  }

  private def buildEvaluationMode(aspEvaluation: ClingoEvaluation, evaluationMode: EvaluationMode) = evaluationMode match {
    case UseFuture(waitingAtMost: Duration) => FutureStreamingAspInterpreter(aspEvaluation, waitingAtMost)
    case _ => aspEvaluation
  }
}

case class EvaluationStrategyConfiguration(aspEvaluation: ClingoEvaluation) {

  def usePull() = PreparedReasonerConfiguration(
    AspPullReasoner(aspEvaluation),
    aspEvaluation.program.intensionalAtoms ++ aspEvaluation.program.signals
  )

  def usePush() = PreparedReasonerConfiguration(
    AspPushReasoner(aspEvaluation),
    aspEvaluation.program.intensionalAtoms ++ aspEvaluation.program.signals
  )

}

case class PreparedReasonerConfiguration(reasoner: Reasoner, restrictTo: Set[Atom]) {

  def withFilter(restrictTo: Set[Atom]) = PreparedReasonerConfiguration(reasoner, restrictTo)

  def seal() = ReasonerWithFilter(reasoner, ResultFilter(restrictTo))

}
