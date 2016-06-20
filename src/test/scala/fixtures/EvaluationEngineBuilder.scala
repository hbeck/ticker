package fixtures

import core.lars.LarsProgram
import engine.EvaluationEngine
import engine.config.BuildEngine

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
    case a: TmsPushEngine => AspBasedTms
    case _ => Clingo
  }

}

trait ClingoPullEngine extends EvaluationEngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).useAsp().withClingo().use().usePull().start()
}

trait ClingoPushEngine extends EvaluationEngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).useAsp().withClingo().use().usePush().start()
}

trait TmsPushEngine extends EvaluationEngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).useAsp().withTms().withRandom(new Random(1)).start()
}
