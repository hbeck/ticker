package jtms.evaluation

import common.Util.stopTime
import core.lars._
import engine.asp.tms.policies.ImmediatelyAddRemovePolicy
import engine.config.{BuildEngine, StartableEngineConfiguration}
import engine.{EvaluationEngine, Result}
import evaluation._
import jtms.JtmsUpdateAlgorithm

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
      "eval_time" -> executionTimes.evaluateTimes.avg,
      "add_time_per_tp" -> (1.0*executionTimes.appendTimes.avg)/(1.0*config.timePoints),
      "eval_time_per_tp" -> (1.0*executionTimes.evaluateTimes.avg)/(1.0*config.timePoints)
    )

    def timeOutput(a: Any) = a match {
      case d: Duration => ((1.0)*d.toMillis)/1000.0 //sec
      //case d: Duration => d.toMillis
      case _ => a
    }

    if (config.withDebug) {
      println()
      outputValues foreach {
        case (k,v) => println(f"$k -> ${timeOutput(v)}")
      }
      println()
    }

    val separator = ";"
    if (config.withHeader) {
      println(outputValues.map(_._1).mkString(separator))
    }

    val values = outputValues.collect {
      case (_, d: Duration) => timeOutput(d)
      case (_, s: String) => f"$s"
    }

    println(values.mkString(separator))
  }

  def run(config: Config): List[ExecutionTimePerRun] = {
    val runIndexes = ((-1 * config.preRuns) to config.runs-1)
    runIndexes.map(evaluateRun(_,config)).toList.drop(config.preRuns)
  }

  var tms: JtmsUpdateAlgorithm = null //debugging

  def evaluateRun(iterationNr: Int, config: Config): ExecutionTimePerRun = {

    if (config.withDebug) { print(" " + iterationNr) }

    val instance = config.makeInstance(iterationNr)
    val builder = BuildEngine.withProgram(instance.larsProgram(instance.windowSize))
    var engine: EvaluationEngine = null

    val initializationTime = stopTime {
      val startableEngine: StartableEngineConfiguration = config.implementation match {
        case Config.CLINGO_PUSH => builder.configure().withClingo().use().usePush()
        case Config.DOYLE_HEURISTICS => {
          tms = config.makeTms(instance)
          builder.configure().withTms().withPolicy(ImmediatelyAddRemovePolicy(tms)).withIncremental()
        }
      }

      engine = startableEngine.start()
    }

    val runSingleTimepoint = runTimepoint(instance, engine, config) _

    val timings: List[ExecutionTimePerTimePoint] = (0 to (config.timePoints-1)) map runSingleTimepoint toList

    val appendStats = StatisticResult.fromMillis(timings.map(_.appendTime))
    val evaluateStats = StatisticResult.fromMillis(timings.map(_.evaluateTime))

    ExecutionTimePerRun(initializationTime, appendStats, evaluateStats)
  }


  def runTimepoint(instance: LarsEvaluationInstance, engine: EvaluationEngine, config: Config)(t: Int): ExecutionTimePerTimePoint = {

    val signals = instance.generateSignalsToAddAt(t)
    val time = TimePoint(t)

    val appendTime = stopTime {
      engine.append(time)(signals: _*)
    }

    var result: Result = null

    val evaluateTime = stopTime {
      result = engine.evaluate(time)
    }

    if (config.withDebug && t==27) {
      println(f"\n rules at t=$t")
      tms.rules foreach println
      println()
    }

    if (config.verifyModel) {
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