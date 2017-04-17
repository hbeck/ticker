package jtms.evaluation

import common.Util.stopTime
import core.lars._
import engine.asp.tms.policies.ImmediatelyAddRemovePolicy
import engine.config.{BuildEngine, StartableEngineConfiguration}
import engine.{EvaluationEngine, Result}
import evaluation._

import scala.concurrent.duration.Duration

/**
  * Created by fm on 24/02/2017.
  */
object LarsEvaluation {

  def main(args: Array[String]): Unit = {
    timings(args)
  }

  def timings(args: Array[String]): Unit = {
    val argMap = Config.buildArgMap(args)
    evaluate(Config(argMap))
  }

  def evaluate(config: Config) = {

    if (config.withDebug) println(config)

    val executionTimes = BatchExecution(run(config))

    val outputValues = Seq(
      "instance" -> config.instanceName,
      "total_time" -> executionTimes.avgTimePerRun,
      "init_time" -> executionTimes.initializationTimes.avg,
      "add_time" -> executionTimes.appendTimes.avg,
      "eval_time" -> executionTimes.evaluateTimes.avg
    )

    val separator = ";"
    if (config.withHeader) {
      println()
      println(outputValues.map(_._1).mkString(separator))
    }

    val values = outputValues.collect {
      case (_, d: Duration) => ((1.0) * d.toMillis) / (1000.0)
      case (_, s: String) => f"$s"
    }

    println(values.mkString(separator))
  }

  def run(config: Config): List[ExecutionTimePerRun] = {
    val runIndexes = ((-1 * config.preRuns) to config.runs-1)
    runIndexes.map(evaluateRun(_,config)).toList.drop(config.preRuns)
  }

  def evaluateRun(iterationNr: Int, config: Config): ExecutionTimePerRun = {

    if (config.withDebug) { print(" " + iterationNr) }

    val instance = config.makeInstance(iterationNr)
    val builder = BuildEngine.withProgram(instance.program)
    var engine: EvaluationEngine = null

    val initializationTime = stopTime {
      val startableEngine: StartableEngineConfiguration = config.implementation match {
        case Config.CLINGO_PUSH => builder.configure().withClingo().use().usePush()
        case Config.DOYLE_HEURISTICS => {
          val tms = config.makeTms(instance)
          builder.configure().withTms().withPolicy(ImmediatelyAddRemovePolicy(tms)).withIncremental()
        }
      }

      engine = startableEngine.start()
    }

    val runSingleTimepoint = runTimepoint(instance, engine, config.verifyModel) _

    val timings: List[ExecutionTimePerTimePoint] = (0 to config.timePoints) map (runSingleTimepoint) toList

    val appendStats = StatisticResult.fromMillis(timings.map(_.appendTime))
    val evaluateStats = StatisticResult.fromMillis(timings.map(_.evaluateTime))

    ExecutionTimePerRun(initializationTime, appendStats, evaluateStats)
  }


  def runTimepoint(instance: LarsEvaluationInstance, engine: EvaluationEngine, verifyModel: Boolean)(t: Int): ExecutionTimePerTimePoint = {

    val signals = instance.generateFactAtomsToAddAt(t)
    val time = TimePoint(t)

    val appendTime = stopTime {
      engine.append(time)(signals: _*)
    }

    var result: Result = null

    val evaluateTime = stopTime {
      result = engine.evaluate(time)
    }

    if (verifyModel) {
      instance.verifyModel(result.get, t)
    }

    ExecutionTimePerTimePoint(time, appendTime, evaluateTime)
  }

}

case class ExecutionTimePerTimePoint(timePoint: TimePoint, appendTime: Long, evaluateTime: Long) {
  val totalTime: Long = appendTime + evaluateTime
}

case class ExecutionTimePerRun(initializationTime: Long, appendTime: StatisticResult, evaluateTime: StatisticResult) {
  val totalRunTime: Long = initializationTime + appendTime.total.toMillis + evaluateTime.total.toMillis
}

case class BatchExecution(runs: List[ExecutionTimePerRun]) {
  val initializationTimes: StatisticResult = StatisticResult.fromMillis(runs.map(_.initializationTime))
  val appendTimes: StatisticResult = StatisticResult.fromExecutionTimes(runs.map(_.appendTime).flatMap(_.executionTimes))
  val evaluateTimes: StatisticResult = StatisticResult.fromExecutionTimes(runs.map(_.evaluateTime).flatMap(_.executionTimes))

  val totalTime: Duration = initializationTimes.total + appendTimes.total + evaluateTimes.total

  val avgTimePerRun: Duration = totalTime / runs.size
}