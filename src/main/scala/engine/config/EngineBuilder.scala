package engine.config

import clingo.{ClingoConversion, ClingoProgramWithLars}
import core.lars.{EngineTimeUnit, LarsProgram}
import engine.EvaluationEngine
import engine.asp._
import engine.asp.oneshot._
import engine.asp.tms.policies.{ImmediatelyAddRemovePolicy, LazyRemovePolicy, TmsPolicy}
import engine.asp.tms.{IncrementalEvaluationEngine, IncrementalRuleMaker, TmsEvaluationEngine}
import engine.config.EvaluationModifier.EvaluationModifier
import engine.config.EvaluationTypes.EvaluationTypes
import jtms.JtmsUpdateAlgorithm
import jtms.algorithms.JtmsDoyle
import jtms.networks.OptimizedNetwork

import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by FM on 14.05.16.
  */
object BuildEngine {
  def withProgram(program: LarsProgram) = EngineEvaluationConfiguration(program)
}

case class EngineEvaluationConfiguration(larsProgram: LarsProgram, withTimePointDuration: EngineTimeUnit = 1 second) {

  def withConfiguration(evaluationType: EvaluationTypes, evaluationModifier: EvaluationModifier) = ArgumentBasedConfiguration(larsProgram, withTimePointDuration).build(evaluationType, evaluationModifier)

  def configure() = ReasoningStrategyConfiguration(larsProgram, withTimePointDuration)

  def withTimePointDuration(duration: EngineTimeUnit) = EngineEvaluationConfiguration(larsProgram, duration)
}

case class ReasoningStrategyConfiguration(program: LarsProgram, withTickSize: EngineTimeUnit) {

  private lazy val aspMapped = PlainLarsToAspMapper(withTickSize)(program)

  def withClingo() = EvaluationModeConfiguration(ClingoConversion.fromLars(aspMapped))

  def withTms(): TmsConfiguration = TmsConfiguration(aspMapped)

}

case class TmsConfiguration(larsProgramEncoding: LarsProgramEncoding, policy: TmsPolicy = LazyRemovePolicy(new JtmsDoyle(new OptimizedNetwork(), new Random))) {

  def withRandom(random: Random) = TmsConfiguration(larsProgramEncoding, ImmediatelyAddRemovePolicy(new JtmsDoyle(new OptimizedNetwork(), random)))

  def useTms(jtms: JtmsUpdateAlgorithm) = TmsConfiguration(larsProgramEncoding, ImmediatelyAddRemovePolicy(jtms))

  def withPolicy(tmsPolicy: TmsPolicy) = TmsConfiguration(larsProgramEncoding, tmsPolicy)

  def withIncremental() = StartableEngineConfiguration(IncrementalEvaluationEngine(IncrementalRuleMaker(larsProgramEncoding), policy))

}

object TmsConfiguration {
  implicit def toEvaluationModeConfig(config: TmsConfiguration): StartableEngineConfiguration = StartableEngineConfiguration(TmsEvaluationEngine(config.larsProgramEncoding, config.policy))
}

case class EvaluationModeConfiguration(clingoProgram: ClingoProgramWithLars) {

  def use(evaluationMode: EvaluationMode = Direct) = {
    val aspEvaluation = buildEvaluationMode(OneShotEvaluationEngine(clingoProgram, StreamingClingoInterpreter(clingoProgram)), evaluationMode)
    EvaluationStrategyConfiguration(aspEvaluation)
  }

  private def buildEvaluationMode(aspEvaluation: OneShotEvaluation, evaluationMode: EvaluationMode) = evaluationMode match {
    case UseFuture(waitingAtMost: Duration) => FutureStreamingAspInterpreter(aspEvaluation, waitingAtMost)
    case _ => aspEvaluation
  }
}

case class EvaluationStrategyConfiguration(aspEvaluation: OneShotEvaluation) {

  def usePull() = StartableEngineConfiguration(AspPullEvaluationEngine(aspEvaluation))

  def usePush() = StartableEngineConfiguration(AspPushEvaluationEngine(aspEvaluation))

}

case class StartableEngineConfiguration(evaluationEngine: EvaluationEngine) {
  def start() = evaluationEngine
}
