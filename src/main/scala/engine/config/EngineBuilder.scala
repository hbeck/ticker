package engine.config

import core.asp.AspProgram
import core.lars.Program
import engine.asp._

/**
  * Created by FM on 14.05.16.
  */
object Engine {
  def withProgram(program: Program) = EngineEvaluationConfiguration(program)
}

case class EngineEvaluationConfiguration(program: Program) {
  def useAsp() = AspEvaluationEngineConfiguration(PlainLarsToAsp(program))

  def useIncremental() = {
    //TODO
  }
}

case class AspEvaluationEngineConfiguration(aspProgram: AspProgram, evaluationMode: EvaluationMode = Direct) {
  var usePull = true
  var useClingo = true

  def withPull() = {
//    val p = AspPushEvaluationEngine()
    usePull = true
    this
  }

  def withPush() = {
    usePull = false
    this
  }

  def withClingo() = {
    useClingo = true
    this
  }

  def withTms() = {
    useClingo = false
    this
  }

  def start() = {

  }
}

//case class ClingoConfiguration
