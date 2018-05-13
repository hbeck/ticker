package evaluation.diss

import core.lars._
import evaluation.diss.instances.traits.Instance
import reasoner.config.{BuildReasoner, PreparedReasonerConfiguration}
import reasoner.incremental.jtms.algorithms.JtmsDoyleHeuristics
import reasoner.{Reasoner, Result}
import util.{DoubleSeries, DurationSeries}

import scala.concurrent.duration.Duration
import scala.util.Random

/**
  * Created by hb on 05.04.18
  */
object DissEvalMain {

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
    "avg_total" -> "",
    "avg_init" -> "",
    "avg_tp" -> "",
    //"avg_proc/sig" -> "",
    //"sig/s" -> "",
    "tp/s" -> ""
  )

  //
  //
  //

  def rnd1(d: Double) = Math.round(10.0 * d)/10.0
  def rnd2(d: Double) = Math.round(100.0 * d)/100.0
  def rnd3(d: Double) = Math.round(1000.0 * d)/1000.0
  def rnd4(d: Double) = Math.round(10000.0 * d)/10000.0
  def rnd5(d: Double) = Math.round(100000.0 * d)/100000.0

  def evaluate(config: Config) = {

    if (config.withDebug) println(config)

    val stats = ExecutionStats(run(config))

    val tp = 1.0*config.timePoints

    val outputValues = Seq(
      Config.KEY_REASONER -> config.reasoner,
      Config.KEY_INSTANCE -> config.instance,
      Config.KEY_TIMEPOINTS -> config.timePoints,
      Config.KEY_WINDOW_SIZE -> config.windowSize,
      "avg_total" -> stats.totalRunTimes.avg,
      "avg_init" -> stats.initializationTimes.avg,
      "avg_tp" -> (1.0*stats.processingTimes.avg)/tp, //tp same for every run
      //"avg_proc/sig" -> stats.processingTimesPerSignal.avg,
      //"sig/s" -> rnd2(stats.signalsPerSecond.avg),
      "tp/s" -> rnd1(10E3*tp/(1.0*stats.processingTimes.avg.toMillis))
    )

    def timeOutput(a: Any) = a match {
      case d: Duration => {
        if (d.isFinite()) { ((1.0) * d.toMillis) / 1000.0 }
        else "INF"
      } //sec
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

  def run(config: Config): Seq[ExecutionStatsPerRun] = {
    val runIndexes = ((-1 * config.preRuns) to config.runs - 1)
    runIndexes.map(evaluateRun(_, config)).drop(config.preRuns)
  }

  var jtms: JtmsDoyleHeuristics = null //debugging

  def evaluateRun(iterationNr: Int, config: Config): ExecutionStatsPerRun = {
    if (config.profiling && iterationNr == 0) {
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

    val start1 = System.currentTimeMillis()

        val preparedReasoner: PreparedReasonerConfiguration = config.reasoner match {
        case Config.CLINGO => builder.configure().withClingo().withDefaultEvaluationMode().usePush()
        case Config.INCREMENTAL => {
          val randomSeed = if (config.overrideRandom) config.fixedRandom else iterationNr
          jtms = JtmsDoyleHeuristics(new Random(randomSeed))
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

    val end1 = System.currentTimeMillis()
    val initializationTime = end1 - start1

    val runSingleTimepoint = runTimepoint(instance, reasoner, config) _

    val statsPerTimePoints: List[ExecutionStatsPerTimePoint] = (0 to (config.timePoints - 1)) map runSingleTimepoint toList

    val appendStats = DurationSeries.fromMillis(statsPerTimePoints.map(_.appendTime))
    val evaluateStats = DurationSeries.fromMillis(statsPerTimePoints.map(_.evaluateTime))
    val signalStats: Seq[Int] = statsPerTimePoints.map(_.nrOfSignals)

    ExecutionStatsPerRun(initializationTime, appendStats, evaluateStats, signalStats)
  }


  def runTimepoint(instance: Instance, reasoner: Reasoner, config: Config)(t: Int): ExecutionStatsPerTimePoint = {

    val signals = instance.generateSignalsToAddAt(t)

    val time = TimePoint(t)

//    val appendTime = stopTime {
//      reasoner.append(time)(signals: _*)
//    }

    val start2 = System.currentTimeMillis()
    reasoner.append(time)(signals: _*)
    val end2 = System.currentTimeMillis()
    val appendTime = end2 - start2

    var result: Result = null

//    val evaluateTime = stopTime {
//      result = reasoner.evaluate(time)
//    }

    val start3 = System.currentTimeMillis()
    result = reasoner.evaluate(time)
    val end3 = System.currentTimeMillis()
    val evaluateTime = end3 - start3

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

    ExecutionStatsPerTimePoint(time, appendTime, evaluateTime, signals.length)
  }

}

case class ExecutionStatsPerTimePoint(timePoint: TimePoint, appendTime: Long, evaluateTime: Long, nrOfSignals: Int) {
  val totalTime: Long = appendTime + evaluateTime
}

//define processing time as append time + evaluate time
case class ExecutionStatsPerRun(initializationTime: Long, appendSeries: DurationSeries, evaluateSeries: DurationSeries, nrOfSignalsSeries: Seq[Int]) {
  val processingSeries: DurationSeries = appendSeries + evaluateSeries
  val processingTime: Long = processingSeries.total.toMillis
  assert(processingTime == appendSeries.total.toMillis + evaluateSeries.total.toMillis)
  val totalRunTime: Long = initializationTime + processingTime
  val totalNrOfSignals: Int = nrOfSignalsSeries.reduce(_ + _)
}

case class ExecutionStats(runs: Seq[ExecutionStatsPerRun]) {

  val initializationTimes: DurationSeries = DurationSeries.fromMillis(runs.map(_.initializationTime))
  val processingTimes: DurationSeries = DurationSeries.fromMillis(runs.map(_.processingTime))
  val totalRunTimes: DurationSeries = DurationSeries.fromMillis(runs.map(_.totalRunTime))

  val processingTimesPerSignal = DurationSeries.fromMillisDoubles(runs.map { r => (1.0*r.processingTime) / (1.0*r.totalNrOfSignals) })
  val signalsPerSecond = DoubleSeries(runs.map{ r => (1000.0* r.totalNrOfSignals) / (1.0*r.processingTime)}) //1000 <= millis!

}