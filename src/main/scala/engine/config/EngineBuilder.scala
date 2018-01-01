package engine.config

import clingo.{ClingoConversion, ClingoProgramWithLars}
import core.Atom
import core.lars.{EngineTimeUnit, LarsProgram}
import engine.asp._
import engine.asp.oneshot._
import engine.asp.tms.policies.{ImmediatelyAddRemovePolicy, JtmsPolicy}
import engine.asp.tms.{IncrementalEngine, IncrementalRuleMaker}
import engine.config.EvaluationModifier.EvaluationModifier
import engine.config.Reasoner.Reasoner
import engine.{ResultFilter, Engine, EngineWithFilter}
import jtms.algorithms.Jtms
import jtms.networks.OptimizedNetwork

import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by FM on 14.05.16.
  */
object BuildEngine {
  def withProgram(program: LarsProgram) = EngineConfiguration(program)
}

case class EngineConfiguration(larsProgram: LarsProgram, withClockTime: EngineTimeUnit = 1 second) {

  def withReasoning(reasoner: Reasoner, evaluationModifier: EvaluationModifier) = ArgumentBasedEngineConfiguration(this).build(reasoner, evaluationModifier)

  def configure() = ReasoningStrategyConfiguration(larsProgram, withClockTime)

  def withClockTime(duration: EngineTimeUnit) = EngineConfiguration(larsProgram, duration)
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

  def withIncremental() = PreparedEngineConfiguration(
    IncrementalEngine(IncrementalRuleMaker(larsProgramEncoding), policy),
    larsProgramEncoding.intensionalAtoms ++ larsProgramEncoding.signals
  )

}

object JtmsConfiguration {
  implicit def toEvaluationModeConfig(config: JtmsConfiguration): PreparedEngineConfiguration =
    PreparedEngineConfiguration(
      IncrementalEngine(IncrementalRuleMaker(config.larsProgramEncoding), config.policy),
      config.larsProgramEncoding.intensionalAtoms ++ config.larsProgramEncoding.signals
    )
}

case class EvaluationModeConfiguration(clingoProgram: ClingoProgramWithLars) {

  def use(evaluationMode: EvaluationMode = Direct) = {
    val aspEvaluation = buildEvaluationMode(OneShotEngine(clingoProgram, StreamingClingoInterpreter(clingoProgram)), evaluationMode)
    EvaluationStrategyConfiguration(aspEvaluation)
  }

  private def buildEvaluationMode(aspEvaluation: OneShotEvaluation, evaluationMode: EvaluationMode) = evaluationMode match {
    case UseFuture(waitingAtMost: Duration) => FutureStreamingAspInterpreter(aspEvaluation, waitingAtMost)
    case _ => aspEvaluation
  }
}

case class EvaluationStrategyConfiguration(aspEvaluation: OneShotEvaluation) {

  def usePull() = PreparedEngineConfiguration(
    AspPullEngine(aspEvaluation),
    aspEvaluation.program.intensionalAtoms ++ aspEvaluation.program.signals
  )

  def usePush() = PreparedEngineConfiguration(
    AspPushEngine(aspEvaluation),
    aspEvaluation.program.intensionalAtoms ++ aspEvaluation.program.signals
  )

}

case class PreparedEngineConfiguration(engine: Engine, restrictTo: Set[Atom]) {

  def withFilter(restrictTo: Set[Atom]) = PreparedEngineConfiguration(engine, restrictTo)

  def seal() = EngineWithFilter(engine, ResultFilter(restrictTo))

}
