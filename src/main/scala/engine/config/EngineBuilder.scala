package engine.config

import clingo.ClingoConversion
import core.lars.Program
import engine.EvaluationEngine
import engine.asp._
import engine.asp.evaluation._

import scala.concurrent.duration.Duration

/**
  * Created by FM on 14.05.16.
  */
object BuildEngine {
  def withProgram(program: Program) = EngineEvaluationConfiguration(program)
}

case class EngineEvaluationConfiguration(program: Program) {
  def useAsp() = AspEvaluationEngineConfiguration(PlainLarsToAsp(program))

  def useIncremental() = {
    //TODO
  }
}


case class AspEvaluationEngineConfiguration(aspProgram: PinnedProgram) {

  def withClingo() = EvaluationModeConfiguration(StreamingClingoInterpreter(ClingoConversion(aspProgram)))

  def withTms() = EvaluationModeConfiguration(TmsEvaluation(aspProgram))

}

case class EvaluationModeConfiguration(streamingAspInterpreter: StreamingAspInterpreter) {

  def use(evaluationMode: EvaluationMode = Direct) = {
    val aspEvaluation = buildEvaluationMode(AspEvaluationEngine(streamingAspInterpreter), evaluationMode)
    EvaluationStrategyConfiguration(aspEvaluation)
  }

  private def buildEvaluationMode(aspEvaluation: AspEvaluation, evaluationMode: EvaluationMode) = evaluationMode match {
    case UseFuture(waitingAtMost: Duration) => FutureStreamingAspInterpreter(aspEvaluation, waitingAtMost)
    case _ => aspEvaluation
  }
}

case class EvaluationStrategyConfiguration(aspEvaluation: AspEvaluation) {
  def usePull() = StartableEngineConfiguration(AspPullEvaluationEngine(aspEvaluation))

  def usePush() = StartableEngineConfiguration(AspPushEvaluationEngine(aspEvaluation))
}

case class StartableEngineConfiguration(evaluationEngine: EvaluationEngine) {
  def start() = evaluationEngine
}
