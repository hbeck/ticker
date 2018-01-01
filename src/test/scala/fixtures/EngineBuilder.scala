package fixtures

import core.lars.LarsProgram
import engine.Engine
import engine.asp.tms.policies.{ImmediatelyAddRemovePolicy, LazyRemovePolicy}
import engine.config.BuildEngine
import jtms.algorithms.{Jtms, JtmsGreedy, JtmsLearn}
import jtms.networks.{OptimizedNetwork, OptimizedNetworkForLearn}

import scala.util.Random

/**
  * Created by FM on 01.06.16.
  */

trait EvaluationType

object Clingo extends EvaluationType

object AspBasedTms extends EvaluationType

trait EngineBuilder {

  case class EngineConfig(evaluationType: EvaluationType, builder: EngineBuilder)

  type EngineBuilder = ((LarsProgram) => Engine)

  val defaultEngine: EngineBuilder

  // needed?
  lazy val defaultEvaluationType = this match {
    case a: TmsDirectPolicyEngine => AspBasedTms
    case a: JtmsGreedyLazyRemovePolicyEngine => AspBasedTms
    case a: JtmsLearnLazyRemovePolicyEngine => AspBasedTms
    case _ => Clingo
  }

}

trait ClingoPullEngine extends EngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).configure().withClingo().use().usePull().seal()
}

trait ClingoPushEngine extends EngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).configure().withClingo().use().usePush().seal()
}

trait TmsDirectPolicyEngine extends EngineBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = new JtmsGreedy(new OptimizedNetwork(), new Random(1))
    tms.doConsistencyCheck = false

    BuildEngine.withProgram(p).configure().withJtms().withPolicy(ImmediatelyAddRemovePolicy(tms)).seal()
  }
}

trait JtmsGreedyLazyRemovePolicyEngine extends EngineBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = new JtmsGreedy(new OptimizedNetwork(), new Random(1))
    tms.doConsistencyCheck = false

    BuildEngine.withProgram(p).configure().withJtms().withPolicy(LazyRemovePolicy(tms)).seal()
  }
}

trait JtmsLearnLazyRemovePolicyEngine extends EngineBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = new JtmsLearn(new OptimizedNetworkForLearn(), new Random(1))
    tms.shuffle = false
    tms.doConsistencyCheck = false

    BuildEngine.withProgram(p).configure().withJtms().withPolicy(LazyRemovePolicy(tms)).seal()
  }
}

trait JtmsIncrementalEngine extends EngineBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = Jtms(new OptimizedNetwork(), new Random(1))
    //tms.doConsistencyCheck = false

    BuildEngine.withProgram(p).configure().withJtms().withPolicy(ImmediatelyAddRemovePolicy(tms)).withIncremental().seal()
  }
}
