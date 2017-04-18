package engine.config

import clingo.{ClingoConversion, ClingoProgramWithLars, ClingoWrapper}
import core.lars.{EngineTimeUnit, LarsProgram}
import engine.EvaluationEngine
import engine.asp._
import engine.asp.oneshot._
import engine.asp.reactive.ReactiveEvaluationEngine
import engine.asp.tms.policies.{ImmediatelyAddRemovePolicy, LazyRemovePolicy, TmsPolicy}
import engine.asp.tms.{IncrementalEvaluationEngine, TmsEvaluationEngine}
import engine.config.EvaluationModifier.EvaluationModifier
import engine.config.EvaluationTypes.EvaluationTypes
import jtms.JtmsUpdateAlgorithm
import jtms.algorithms.JtmsGreedy
import jtms.networks.OptimizedNetwork

import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by FM on 14.05.16.
  */
object BuildEngine {
  def withProgram(program: LarsProgram) = EngineEvaluationConfiguration(program)
}

case class EngineEvaluationConfiguration(larsProgram: LarsProgram, withTickSize: EngineTimeUnit = 1 second) {

  def withConfiguration(evaluationType: EvaluationTypes, evaluationModifier: EvaluationModifier) = ArgumentBasedConfiguration(larsProgram, withTickSize).build(evaluationType, evaluationModifier)

  //TODO hb: assuming correct understanding: due to the new mapping, we should simply have a "LarsToAsp" mapping, since the result
  //is no longer "pinned" (in the sense that only some atoms get an additional time argument)
  def configure() = AspEngineEvaluationConfiguration(larsProgram, withTickSize)

  def withTickSize(tickSize: EngineTimeUnit) = EngineEvaluationConfiguration(larsProgram, tickSize)
}

//TODO hb name misleading: if we use TMS, why would we call it "AspEngine"? the name hints at something like clingo or dlv
case class AspEngineEvaluationConfiguration(program: LarsProgram, withTickSize: EngineTimeUnit) {

  private lazy val aspMapped = PlainLarsToAspMapper(withTickSize)(program)
  private lazy val reactiveMapped = PlainLarsToReactiveMapper(withTickSize)(program)

  def withClingo() = EvaluationModeConfiguration(ClingoConversion.fromLars(aspMapped))

  def withReactive() = ReactiveClingoConfiguration(reactiveMapped)

  def withTms(): TmsConfiguration = {
    TmsConfiguration(aspMapped)
  }

}

case class TmsConfiguration(larsProgramEncoding: LarsProgramEncoding, policy: TmsPolicy = LazyRemovePolicy(new JtmsGreedy(new OptimizedNetwork(), new Random))) {

  def withRandom(random: Random) = TmsConfiguration(larsProgramEncoding, ImmediatelyAddRemovePolicy(new JtmsGreedy(new OptimizedNetwork(), random)))

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

case class ReactiveClingoConfiguration(program: LarsProgramEncoding, wrapper: ClingoWrapper = ClingoWrapper()) {
  def withWrapper(wrapper: ClingoWrapper) = ReactiveClingoConfiguration(program, wrapper)

  def startable() = StartableEngineConfiguration(ReactiveEvaluationEngine(program, wrapper))
}

object ReactiveClingoConfiguration {
  implicit def toStartable(config: ReactiveClingoConfiguration): StartableEngineConfiguration = config.startable()
}


case class StartableEngineConfiguration(evaluationEngine: EvaluationEngine) {
  def start() = evaluationEngine //TODO hb? why is called start?
}
