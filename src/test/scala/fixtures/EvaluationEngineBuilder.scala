package fixtures

import core.lars.LarsProgram
import engine.EvaluationEngine
import engine.asp.tms.policies.{ImmediatelyAddRemovePolicy, LazyRemovePolicy}
import engine.config.BuildEngine
import jtms.algorithms.{JtmsGreedy, JtmsLearn}
import jtms.networks.{OptimizedNetwork, OptimizedNetworkForLearn}

import scala.util.Random

/**
  * Created by FM on 01.06.16.
  */

trait EvaluationType

object Clingo extends EvaluationType

object AspBasedTms extends EvaluationType

trait EvaluationEngineBuilder {

  case class EngineConfig(evaluationType: EvaluationType, builder: EngineBuilder)

  type EngineBuilder = ((LarsProgram) => EvaluationEngine)


  val defaultEngine: EngineBuilder

  // needed?
  lazy val defaultEvaluationType = this match {
    case a: TmsDirectPolicyEngine => AspBasedTms
    case a: JtmsGreedyLazyRemovePolicyEngine => AspBasedTms
    case a: JtmsLearnLazyRemovePolicyEngine => AspBasedTms
    case _ => Clingo
  }

}

trait ClingoPullEngine extends EvaluationEngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).configure().withClingo().use().usePull().start()
}

trait ClingoPushEngine extends EvaluationEngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).configure().withClingo().use().usePush().start()
}

trait ReactiveClingoEngine extends EvaluationEngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).configure().withReactive().startable().start()
}

trait TmsDirectPolicyEngine extends EvaluationEngineBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = JtmsGreedy(new OptimizedNetwork(), new Random(1))
    tms.doConsistencyCheck = false

    BuildEngine.withProgram(p).configure().withTms().withPolicy(ImmediatelyAddRemovePolicy(tms)).start()
  }
}

trait JtmsGreedyLazyRemovePolicyEngine extends EvaluationEngineBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = JtmsGreedy(new OptimizedNetwork(), new Random(1))
    tms.doConsistencyCheck = false

    BuildEngine.withProgram(p).configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }
}

trait JtmsLearnLazyRemovePolicyEngine extends EvaluationEngineBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = new JtmsLearn(new OptimizedNetworkForLearn(), new Random(1))
    tms.shuffle = false
    tms.doConsistencyCheck = false

    BuildEngine.withProgram(p).configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }
}

trait JtmsIncrementalEngine extends EvaluationEngineBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = new JtmsGreedy(new OptimizedNetwork(), new Random(1))
    tms.shuffle = true
    tms.doConsistencyCheck = false

    BuildEngine.withProgram(p).configure().withTms().withPolicy(ImmediatelyAddRemovePolicy(tms)).withIncremental().start()
  }
}
