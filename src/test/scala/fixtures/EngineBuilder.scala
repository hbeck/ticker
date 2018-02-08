package fixtures

import core.lars.LarsProgram
import reasoner.Reasoner
import reasoner.config.BuildReasoner
import reasoner.incremental.jtms.algorithms.Jtms
import reasoner.incremental.jtms.networks.OptimizedNetwork

import scala.util.Random

/**
  * Created by FM on 01.06.16.
  */

trait EvaluationType

object Clingo extends EvaluationType

object AspBasedTms extends EvaluationType

trait EngineBuilder {

  case class EngineConfig(evaluationType: EvaluationType, builder: EngineBuilder)

  type EngineBuilder = ((LarsProgram) => Reasoner)

  val defaultEngine: EngineBuilder

  // needed?
  lazy val defaultEvaluationType = this match {
    case _ => Clingo
  }

}

trait ClingoPullEngine extends EngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildReasoner.withProgram(p).configure().withClingo().withDefaultEvaluationMode().usePull().seal()
}

trait ClingoPushEngine extends EngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildReasoner.withProgram(p).configure().withClingo().withDefaultEvaluationMode().usePush().seal()
}

trait JtmsIncrementalEngine extends EngineBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = Jtms(new OptimizedNetwork(), new Random(1))
    //tms.doConsistencyCheck = false

    BuildReasoner.withProgram(p).configure().withIncremental().withJtms(tms).use().seal()
  }
}
