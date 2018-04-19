package evaluation.diss

import common.Util.stopTime
import core.lars._
import reasoner.config.{BuildReasoner, PreparedReasonerConfiguration}
import reasoner.incremental.jtms.algorithms.JtmsDoyleHeuristics
import reasoner.{Reasoner, Result}
import util.DurationSeries

import scala.concurrent.duration.Duration
import scala.util.Random

/**
  * Created by hb on 05.04.18
  */
object DissEvalMain {

  val profiling = false

  def main(args: Array[String]): Unit = {
    timings(args)
  }

  def timings(args: Array[String]): Unit = {
    val argMap = Config.buildArgMap(args)
    val config = Config(argMap)
    if (config.headerOnly) {
      printHeader(dummyOutputValues)
    } else {
      evaluate(config)
    }
  }

  val separator = "\t"

  def printHeader(outputValues: Seq[(String, Any)]): Unit = {
    println(outputValues.map(_._1).mkString(separator))
  }

  val dummyOutputValues = Seq(
    Config.KEY_REASONER -> "",
    Config.KEY_INSTANCE -> "",
    Config.KEY_TIMEPOINTS -> "",
    Config.KEY_WINDOW_SIZE -> "",
    "total_time" -> "",
    "init_time" -> "",
    "add_time" -> "",
    "eval_time_per_tp" -> "",
    "tp_per_sec" -> ""
    //"eval_time" -> executionTimes.evaluateTimes.avg,
    //"add_time_per_tp" -> ""
    //"eval_time_per_tp" -> (1.0*executionTimes.evaluateTimes.avg)/(1.0*config.timePoints)
  )

  //
  //
  //

  def evaluate(config: Config) = {

    if (config.withDebug) println(config)

    val executionTimes = ExecutionTimes(run(config))

    val outputValues = Seq(
      Config.KEY_REASONER -> config.reasoner,
      Config.KEY_INSTANCE -> config.instance,
      Config.KEY_TIMEPOINTS -> config.timePoints,
      Config.KEY_WINDOW_SIZE -> config.windowSize,
      "total_time" -> executionTimes.avgTotalTimePerRun,
      "init_time" -> executionTimes.initializationTimes.avg,
      "add_time" -> executionTimes.appendTimes.avg,
      "eval_time_per_tp" -> (1.0*executionTimes.avgEvaluationTimePerRun.toSeconds)/(1.0*config.timePoints),
      "tp_per_sec" -> (1.0*config.timePoints)/(1.0*executionTimes.avgEvaluationTimePerRun.toSeconds)
      //"eval_time" -> executionTimes.evaluateTimes.avg,
      //"add_time_per_tp" -> (1.0*executionTimes.appendTimes.avg)/(1.0*config.timePoints)
      //"eval_time_per_tp" -> (1.0*executionTimes.evaluateTimes.avg)/(1.0*config.timePoints)
    )

    def timeOutput(a: Any) = a match {
      case d: Duration => ((1.0) * d.toMillis) / 1000.0 //sec
      //case d: Duration => d.toMillis
      case _ => a
    }

    if (config.withDebug) {
      println()
      outputValues foreach {
        case (k, d: Duration) => println(f"$k -> ${timeOutput(d)}")
        case (k, i: Integer) => println(f"$k -> $i")
        case (k, s: String) => println(f"$k -> $s")
        case (k, s) => println(f"$k -> $s")
      }
      println()
    }
    if (config.withHeader) {
      printHeader(outputValues)
    }

    val values = outputValues.collect {
      case (_, d: Duration) => timeOutput(d)
      case (_, i: Integer) => f"$i"
      case (_, s: String) => f"$s"
      case (_, d: Double) => f"$d"
    }

    println(values.mkString(separator))
  }

  def run(config: Config): List[ExecutionTimePerRun] = {
    val runIndexes = ((-1 * config.preRuns) to config.runs - 1)
    runIndexes.map(evaluateRun(_, config)).toList.drop(config.preRuns)
  }

  var jtms: JtmsDoyleHeuristics = null //debugging

  def evaluateRun(iterationNr: Int, config: Config): ExecutionTimePerRun = {
    if (profiling && iterationNr == 0) {
      println("waiting to start profiling, press return")
      scala.io.StdIn.readLine()
      println("running")
    }

    if (config.withDebug) {
      print(" " + iterationNr)
    }

    val instance = config.makeInstance(iterationNr)
    val builder = BuildReasoner.withProgram(instance.program)
    var reasoner: Reasoner = null

    val initializationTime = stopTime {
      val preparedReasoner: PreparedReasonerConfiguration = config.reasoner match {
        case Config.CLINGO => builder.configure().withClingo().withDefaultEvaluationMode().usePush()
        case Config.INCREMENTAL => {
          jtms = JtmsDoyleHeuristics(new Random(iterationNr))
          if (config.semantics_checks) {
            jtms.doConsistencyCheck = true
            jtms.doJtmsSemanticsCheck = true
            jtms.doSelfSupportCheck = true
          }
          builder.configure().withIncremental().withJtms(jtms).use()
        }
        case x => throw new RuntimeException("reasoning mode not supported: "+x)
      }
      reasoner = preparedReasoner.seal()
    }

    val runSingleTimepoint = runTimepoint(instance, reasoner, config) _

    val timings: List[ExecutionTimePerTimePoint] = (0 to (config.timePoints - 1)) map runSingleTimepoint toList

    val appendStats = DurationSeries.fromMillis(timings.map(_.appendTime))
    val evaluateStats = DurationSeries.fromMillis(timings.map(_.evaluateTime))

    ExecutionTimePerRun(initializationTime, appendStats, evaluateStats)
  }


  def runTimepoint(instance: Instance, reasoner: Reasoner, config: Config)(t: Int): ExecutionTimePerTimePoint = {

    val signals = instance.generateSignalsToAddAt(t)

    val time = TimePoint(t)

    val appendTime = stopTime {
      reasoner.append(time)(signals: _*)
    }

    var result: Result = null

    val evaluateTime = stopTime {
      result = reasoner.evaluate(time)
    }

    if (t == config.printRulesAt && config.reasoner == Config.INCREMENTAL) {
      println(f"\ntms rules at t=$t")
      jtms.rules foreach println
      println()
    }

    if (config.verifyModels) {
      instance.verifyOutput(result, t)
    }

    if (t == config.printModelAt) {
      println(f"\nmodel at t=$t")
      result.get match {
        case None => println("(none)")
        case Some(model) => println(model)
      }
      println()
    }

    ExecutionTimePerTimePoint(time, appendTime, evaluateTime)
  }

}

case class ExecutionTimePerTimePoint(timePoint: TimePoint, appendTime: Long, evaluateTime: Long) {
  val totalTime: Long = appendTime + evaluateTime
}

case class ExecutionTimePerRun(initializationTime: Long, appendTime: DurationSeries, evaluateTime: DurationSeries) {
  val totalRunTime: Long = initializationTime + appendTime.total.toMillis + evaluateTime.total.toMillis
}

case class ExecutionTimes(runs: List[ExecutionTimePerRun]) {

  val initializationTimes: DurationSeries = DurationSeries.fromMillis(runs.map(_.initializationTime))
  val appendTimes: DurationSeries = DurationSeries.fromExecutionTimes(runs.map(_.appendTime).flatMap(_.durations))
  val evaluateTimes: DurationSeries = DurationSeries.fromExecutionTimes(runs.map(_.evaluateTime).flatMap(_.durations))

  val totalTime: Duration = initializationTimes.total + appendTimes.total + evaluateTimes.total

  val avgTotalTimePerRun: Duration = totalTime / (1.0*runs.size)

  val avgEvaluationTimePerRun: Duration = (appendTimes.total + evaluateTimes.total) / (1.0*runs.size)

}