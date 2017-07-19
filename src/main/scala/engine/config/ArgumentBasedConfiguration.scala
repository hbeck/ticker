package engine.config

import core.lars.{EngineTimeUnit, LarsProgram}
import engine.EvaluationEngine
import engine.asp.tms.policies.{ImmediatelyAddRemovePolicy, LazyRemovePolicy}
import engine.config.EvaluationModifier.EvaluationModifier
import engine.config.Reasoner.Reasoner
import jtms.TruthMaintenanceNetwork
import jtms.algorithms.{JtmsDoyle, JtmsGreedy, JtmsLearn}
import jtms.networks.{OptimizedNetwork, OptimizedNetworkForLearn}

import scala.util.Random

/**
  * Created by FM on 29.08.16.
  */
object Reasoner extends Enumeration {
  type Reasoner = Value
  val Ticker, Clingo = Value
}

object EvaluationModifier extends Enumeration {
  type EvaluationModifier = Value
  val GreedyLazyRemove, GreedyIncremental, DoyleLazyRemove, Learn, DoyleIncremental, Push, Pull = Value
}


case class ArgumentBasedConfiguration(config: EngineEvaluationConfiguration) {

  def build(evaluationType: Reasoner, evaluationModifier: EvaluationModifier) = buildEngine(evaluationType, evaluationModifier)

  def buildEngine(evaluationType: Reasoner,
                  evaluationModifier: EvaluationModifier,
                  network: TruthMaintenanceNetwork = new OptimizedNetwork(),
                  random: Random = new Random(1)): Option[EvaluationEngine] = {

    if (evaluationType == Reasoner.Ticker) {
      if (evaluationModifier == EvaluationModifier.GreedyLazyRemove) {
        return Some(greedyTms(config, network, random))
      } else if (evaluationModifier == EvaluationModifier.GreedyIncremental) {
        return Some(greedyTmsIncremental(config, network, random)) //TODO
      } else if (evaluationModifier == EvaluationModifier.DoyleLazyRemove) {
        return Some(doyleTms(config, network, random))
      } else if (evaluationModifier == EvaluationModifier.Learn) {
        return Some(learnTms(config, new OptimizedNetworkForLearn(), random))
      } else if (evaluationModifier == EvaluationModifier.DoyleIncremental) {
        return Some(incrementalTms(config, network, random))
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

  def greedyTms(config: EngineEvaluationConfiguration, network: TruthMaintenanceNetwork = new OptimizedNetwork(), random: Random = new Random(1)) = {
    val tms = new JtmsGreedy(network, random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    config.configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }

  def greedyTmsIncremental(config: EngineEvaluationConfiguration, network: TruthMaintenanceNetwork = new OptimizedNetwork(), random: Random = new Random(1)) = {
    val tms = new JtmsGreedy(network, random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    config.configure().withTms().withPolicy(ImmediatelyAddRemovePolicy(tms)).withIncremental().start()
  }

  def doyleTms(config: EngineEvaluationConfiguration, network: TruthMaintenanceNetwork = new OptimizedNetwork(), random: Random = new Random(1)) = {
    val tms = new JtmsDoyle(network, random)
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    config.configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }

  def learnTms(config: EngineEvaluationConfiguration, network: OptimizedNetworkForLearn = new OptimizedNetworkForLearn(), random: Random = new Random(1)) = {
    val tms = new JtmsLearn(network, random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    config.configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }

  def incrementalTms(config: EngineEvaluationConfiguration, network: TruthMaintenanceNetwork, random: Random) = {
    val tms = new JtmsDoyle(network, random)
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false
    tms.doSelfSupportCheck = false
    tms.doConsistencyCheck = false
    config.configure().withTms().withPolicy(ImmediatelyAddRemovePolicy(tms)).withIncremental().start()
  }


  def clingoPush(config: EngineEvaluationConfiguration) = {
    config.configure().withClingo().use().usePush().start()
  }

  def clingoPull(config: EngineEvaluationConfiguration) = {
    config.configure().withClingo().use().usePull().start()
  }
}