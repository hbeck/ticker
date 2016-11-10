package engine.config

import core.lars.{EngineTick, LarsProgram}
import engine.EvaluationEngine
import engine.asp.tms.policies.LazyRemovePolicy
import engine.config.EvaluationModifier.EvaluationModifier
import engine.config.EvaluationTypes.EvaluationTypes
import jtms.JtmsStorage
import jtms.algorithms.{JtmsDoyle, JtmsGreedy, JtmsLearn}
import jtms.storage.OptimizedJtmsStorage

import scala.util.Random

/**
  * Created by FM on 29.08.16.
  */
object EvaluationTypes extends Enumeration {
  type EvaluationTypes = Value
  val Tms, Clingo = Value
}

object EvaluationModifier extends Enumeration {
  type EvaluationModifier = Value
  val Greedy, Doyle, Learn, Push, Pull = Value
}


case class ArgumentBasedConfiguration(program: LarsProgram, tickSize: EngineTick) {

  def build(evaluationType: EvaluationTypes, evaluationModifier: EvaluationModifier) = buildEngine(evaluationType, evaluationModifier)

  def buildEngine(evaluationType: EvaluationTypes,
                  evaluationModifier: EvaluationModifier,
                  jtms: JtmsStorage = new OptimizedJtmsStorage(),
                  random: Random = new Random(1)): Option[EvaluationEngine] = {

    val config = BuildEngine.withProgram(program).withTickSize(tickSize)

    if (evaluationType == EvaluationTypes.Tms) {
      if (evaluationModifier == EvaluationModifier.Greedy) {
        return Some(greedyTms(config, jtms, random))
      } else if (evaluationModifier == EvaluationModifier.Doyle) {
        return Some(doyleTms(config, jtms, random))
      } else if (evaluationModifier == EvaluationModifier.Learn) {
        return Some(learnTms(config, new OptimizedJtmsStorage(), random))
      }
    } else if (evaluationType == EvaluationTypes.Clingo) {
      if (evaluationModifier == EvaluationModifier.Push) {
        return Some(clingoPush(config))
      } else if (evaluationModifier == EvaluationModifier.Pull) {
        return Some(clingoPull(config))
      }
    }

    None
  }

  def greedyTms(config: EngineEvaluationConfiguration, jtms: JtmsStorage = new OptimizedJtmsStorage(), random: Random = new Random(1)) = {
    val tms = JtmsGreedy(jtms, random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    config.configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }

  def doyleTms(config: EngineEvaluationConfiguration, jtms: JtmsStorage = new OptimizedJtmsStorage(), random: Random = new Random(1)) = {
    val tms = JtmsDoyle(jtms, random)
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    config.configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }

  def learnTms(config: EngineEvaluationConfiguration, jtms: OptimizedJtmsStorage = new OptimizedJtmsStorage(), random: Random = new Random(1)) = {
    val tms = new JtmsLearn(jtms, random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    config.configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }

  def clingoPush(config: EngineEvaluationConfiguration) = {
    config.configure().withClingo().use().usePush().start()
  }

  def clingoPull(config: EngineEvaluationConfiguration) = {
    config.configure().withClingo().use().usePull().start()
  }
}