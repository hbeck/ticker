package fixtures

import core.lars.LarsProgram
import engine.EvaluationEngine
import engine.config.BuildEngine

import scala.util.Random

/**
  * Created by FM on 01.06.16.
  */


trait EvaluationEngineBuilder {
  type EngineBuilder = ((LarsProgram) => EvaluationEngine)
  val defaultEngine: EngineBuilder
}

trait ClingoPullEngine extends EvaluationEngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).useAsp().withClingo().use().usePull().start()
}

trait ClingoPushEngine extends EvaluationEngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).useAsp().withClingo().use().usePush().start()
}

trait TmsPushEngine extends EvaluationEngineBuilder {
  val defaultEngine = (p: LarsProgram) => BuildEngine.withProgram(p).useAsp().withTms(new Random(1)).use().usePush().start()
}
