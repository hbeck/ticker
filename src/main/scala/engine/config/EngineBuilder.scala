package engine.config

import clingo.{ClingoConversion, ClingoProgramWithLars}
import core.Atom
import core.lars.{EngineTimeUnit, LarsProgram}
import engine.asp._
import engine.asp.oneshot._
import engine.asp.tms.policies.{ImmediatelyAddRemovePolicy, JtmsPolicy}
import engine.asp.tms.{IncrementalEvaluationEngine, IncrementalRuleMaker}
import engine.config.EvaluationModifier.EvaluationModifier
import engine.config.Reasoner.Reasoner
import engine.{AtomResultFilter, EvaluationEngine, EvaluationEngineWithResultFilter}
import jtms.algorithms.Jtms
import jtms.networks.OptimizedNetwork

import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by FM on 14.05.16.
  */
object BuildEngine {
  def withProgram(program: LarsProgram) = EngineEvaluationConfiguration(program) //TODO hb why experimental.evaluation?
}

case class EngineEvaluationConfiguration(larsProgram: LarsProgram, withTimePointDuration: EngineTimeUnit = 1 second) {

  def withConfiguration(evaluationType: Reasoner, evaluationModifier: EvaluationModifier) = ArgumentBasedConfiguration(this).build(evaluationType, evaluationModifier)

  def configure() = ReasoningStrategyConfiguration(larsProgram, withTimePointDuration)

  def withTimePointDuration(duration: EngineTimeUnit) = EngineEvaluationConfiguration(larsProgram, duration)
}

case class ReasoningStrategyConfiguration(program: LarsProgram, withTickSize: EngineTimeUnit) {

  private lazy val aspMapped = PlainLarsToAspMapper(withTickSize)(program)

  def withClingo() = EvaluationModeConfiguration(ClingoConversion.fromLars(aspMapped))

  def withJtms(): JtmsConfiguration = JtmsConfiguration(aspMapped)

}

case class JtmsConfiguration(larsProgramEncoding: LarsProgramEncoding, policy: JtmsPolicy = ImmediatelyAddRemovePolicy()) {

  def withRandom(random: Random) = JtmsConfiguration(larsProgramEncoding, ImmediatelyAddRemovePolicy(Jtms(new OptimizedNetwork(), random)))

  def useJtms(jtms: Jtms) = JtmsConfiguration(larsProgramEncoding, ImmediatelyAddRemovePolicy(jtms))

  def withPolicy(jtmsPolicy: JtmsPolicy) = JtmsConfiguration(larsProgramEncoding, jtmsPolicy)

  def withIncremental() = StartableEngineConfiguration(
    IncrementalEvaluationEngine(IncrementalRuleMaker(larsProgramEncoding), policy),
    larsProgramEncoding.intensionalAtoms ++ larsProgramEncoding.signals
  )

}

object JtmsConfiguration {
  implicit def toEvaluationModeConfig(config: JtmsConfiguration): StartableEngineConfiguration =
    StartableEngineConfiguration(
      IncrementalEvaluationEngine(IncrementalRuleMaker(config.larsProgramEncoding), config.policy),
      config.larsProgramEncoding.intensionalAtoms ++ config.larsProgramEncoding.signals
    )
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

  def usePull() = StartableEngineConfiguration(
    AspPullEvaluationEngine(aspEvaluation),
    aspEvaluation.program.intensionalAtoms ++ aspEvaluation.program.signals
  )

  def usePush() = StartableEngineConfiguration(
    AspPushEvaluationEngine(aspEvaluation),
    aspEvaluation.program.intensionalAtoms ++ aspEvaluation.program.signals
  )

}

case class StartableEngineConfiguration(evaluationEngine: EvaluationEngine, restrictTo: Set[Atom]) {

  def filterTo(restrictTo: Set[Atom]) = StartableEngineConfiguration(evaluationEngine, restrictTo)

  def start() = EvaluationEngineWithResultFilter(evaluationEngine, AtomResultFilter(restrictTo))

  def startWithoutFilter() = evaluationEngine
}
