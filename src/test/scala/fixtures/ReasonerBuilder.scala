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

trait ReasonerBuilder {

  case class EngineConfig(evaluationType: EvaluationType, builder: ReasonerBuilder)

  type ReasonerBuilder = ((LarsProgram) => Reasoner)

  val defaultEngine: ReasonerBuilder

  // needed?
  lazy val defaultEvaluationType = this match {
    case _ => Clingo
  }

}

trait ClingoPullReasoner extends ReasonerBuilder {
  val defaultEngine = (p: LarsProgram) => BuildReasoner.withProgram(p).configure().withClingo().withDefaultEvaluationMode().usePull().seal()
}

trait ClingoPushReasoner extends ReasonerBuilder {
  val defaultEngine = (p: LarsProgram) => BuildReasoner.withProgram(p).configure().withClingo().withDefaultEvaluationMode().usePush().seal()
}

trait JtmsIncrementalReasoner extends ReasonerBuilder {

  val defaultEngine = (p: LarsProgram) => {
    val tms = Jtms(new OptimizedNetwork(), new Random(1))
    //tms.doConsistencyCheck = false

    BuildReasoner.withProgram(p).configure().withIncremental().withJtms(tms).use().seal()
  }
}
