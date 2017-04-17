package jtms.evaluation

import common.Util.stopTime
import core._
import core.lars._
import engine.asp.tms.policies.ImmediatelyAddRemovePolicy
import engine.config.{BuildEngine, StartableEngineConfiguration}
import engine.{EvaluationEngine, Result}
import evaluation._
import jtms.JtmsUpdateAlgorithm
import jtms.algorithms.{JtmsDoyle, JtmsDoyleHeuristics, JtmsGreedy, JtmsLearn}
import jtms.networks.{OptimizedNetwork, OptimizedNetworkForLearn, SimpleNetwork}

import scala.concurrent.duration.Duration
import scala.util.Random

/**
  * Created by fm on 24/02/2017.
  */
object StreamingEvaluation {

  def main(args: Array[String]): Unit = {
    timings(args)
  }

  //implementations:
  val DOYLE_SIMPLE = "DoyleSimple"
  val DOYLE = "Doyle"
  val DOYLE_HEURISTICS = "DoyleHeur"
  val GREEDY = "Greedy"
  val LEARN = "Learn"
  val CLINGO_PUSH = "ClingoPush"
  val CLINGO_PULL = "ClingoPush"


  //known instances:
  val MMEDIA_DET = "mmediaDet"
  val MMEDIA_NONDET = "mmediaNonDet"
  val CACHE_HOPS1 = "cacheHops1"
  val CACHE_HOPS2 = "cacheHops2"

  var INSTANCE_NAME = "inst"
  var IMPL = "tms"
  var PRE_RUNS = "pre"
  var RUNS = "runs"
  var TIMEPOINTS = "tp"
  var MODEL_RATIO = "ratio"
  var WINDOW_SIZE = "winsize"
  //
  var ITEMS = "items"
  //* (for cache hops)
  //
  var PRINT_RULES = "printRules"
  var INDICATE_TIMEPOINTS = "dots"
  var SEMANTICS_CHECKS = "checks"

  def timings(args: Array[String]): Unit = {

    var argMap = buildArgMap(args)

    def defaultArg(key: String, value: String) = {
      if (!argMap.contains(key)) {
        argMap = argMap + (key -> value)
      }
    }

    defaultArg(INSTANCE_NAME, CACHE_HOPS2)
    defaultArg(IMPL, DOYLE_HEURISTICS)
    defaultArg(PRE_RUNS, "0")
    defaultArg(RUNS, "1")
    defaultArg(TIMEPOINTS, "2")
    defaultArg(MODEL_RATIO, "false")
    defaultArg(WINDOW_SIZE, "2")
    //
    defaultArg(ITEMS, "1")
    //
    defaultArg(PRINT_RULES, "false")
    defaultArg(INDICATE_TIMEPOINTS, "false")
    defaultArg(SEMANTICS_CHECKS, "false")

    evaluate(Config(argMap))

  }

  def buildArgMap(args: Array[String]): Map[String, String] = {

    if (args.length % 2 == 1) {
      println("need even number of args. given: " + args)
      System.exit(1)
    }
    if (args.length == 0) {
      return Map()
    }

    var argMap = Map[String, String]()
    for (i <- 0 to args.length / 2 - 1) {
      argMap = argMap + (args(2 * i) -> args(2 * i + 1))
    }
    argMap

  }

  case class Config(args: Map[String, String]) {

    val instanceName = args(INSTANCE_NAME)
    val preRuns = Integer.parseInt(args(PRE_RUNS))
    val runs = Integer.parseInt(args(RUNS))
    val modelRatio: Boolean = (args(MODEL_RATIO) == "true")
    val timePoints = Integer.parseInt(args(TIMEPOINTS))
    val windowSize = Integer.parseInt(args(WINDOW_SIZE))
    val nrOfItems = Integer.parseInt(args(ITEMS))
    val withDebug = true
    val withHeader = true
    val implementation = args(IMPL)
    val verifyModel = true

    def makeInstance(iterationNr: Int): EvaluationInstance = {
      val random = new Random(iterationNr)
      args(INSTANCE_NAME) match {
        //        case CACHE_HOPS1 => {
        //          val printRules: Boolean = (args(PRINT_RULES) == "true")
        //          CacheHopsEvalInst1(timePoints,nrOfItems,printRules,random)
        //        }
        //        case CACHE_HOPS2 => {
        //          val printRules: Boolean = (args(PRINT_RULES) == "true")
        //          CacheHopsEvalInst2(timePoints,nrOfItems,printRules,random)
        //        }
        case MMEDIA_DET => MMediaDeterministic(windowSize, timePoints, random)
        //        case MMEDIA_NONDET => MMediaNonDeterministicEvalInst(windowSize, timePoints, random)
        case s => println(f"unknown instance name $s"); throw new RuntimeException
      }
    }

    def makeTms(inst: EvaluationInstance): JtmsUpdateAlgorithm = {
      val tms = args(IMPL) match {
        case DOYLE_SIMPLE => new JtmsDoyle(new SimpleNetwork(), inst.random)
        case DOYLE => new JtmsDoyle(new OptimizedNetwork(), inst.random)
        case DOYLE_HEURISTICS => new JtmsDoyleHeuristics(new OptimizedNetwork(), inst.random)
        case GREEDY => new JtmsGreedy(new OptimizedNetwork(), inst.random)
        case LEARN => new JtmsLearn(new OptimizedNetworkForLearn(), inst.random)
        case _ => throw new RuntimeException("unknown tms: "+args(IMPL))
      }

      if ((tms.isInstanceOf[JtmsDoyle]) && args(SEMANTICS_CHECKS) == "true") {
        tms.asInstanceOf[JtmsDoyle].doConsistencyCheck = true
        tms.asInstanceOf[JtmsDoyle].doJtmsSemanticsCheck = true
        tms.asInstanceOf[JtmsDoyle].doSelfSupportCheck = true
      }

      tms
    }

    def isSomeDoyle() = {
      args(IMPL).toLowerCase().contains("doyle")
    }

  }

  def evaluate(config: Config) = {

    val executionTimes = BatchExecutionTimes(run(config))

    val outputValues = Seq(
      "instance" -> config.instanceName,
      "total_time" -> executionTimes.avgTimePerRun,
      "init_time" -> executionTimes.initializationTimes.avg,
      "add_time" -> executionTimes.appendTimes.avg,
      "eval_time" -> executionTimes.evaluateTimes.avg
    )

    val separator = ";"
    if (config.withHeader) {
      println(outputValues.map(_._1).mkString(separator))
    }

    val values = outputValues.collect {
      case (_, d: Duration) => ((1.0) * d.toMillis) / (1000.0)
      case (_, s: String) => f"$s"
    }

    println(values.mkString(separator))
  }

  def run(config: Config): List[ExecutionTimePerRun] = {

    val runResults:Seq[ExecutionTimePerRun] = ((-1 * config.preRuns) to config.runs) map { i =>
      if (config.withDebug) {
        if (i < 0) { println(f"# pre-run $i") }
        else { println(f"# run $i") }
      }
      evaluateRun(i, config)
    }

    runResults.toList

  }


  def evaluateRun(iterationNr: Int, config: Config): ExecutionTimePerRun = {

    val instance = config.makeInstance(iterationNr)

    val builder = BuildEngine.withProgram(instance.program)

    var engine: EvaluationEngine = null

    val initializationTime = stopTime {

      val startableEngine: StartableEngineConfiguration = config.implementation match {
        case CLINGO_PUSH => builder.configure().withClingo().use().usePush()
        case DOYLE_HEURISTICS => {
          val tms = config.makeTms(instance)
          builder.configure().withTms().withPolicy(ImmediatelyAddRemovePolicy(tms)).withIncremental()
        }
      }

      engine = startableEngine.start()

    }

    val timepoints = Seq.range(0, config.timePoints)

    val runSingleTimepoint = runTimepoint(instance, engine, config.verifyModel) _

    val timings: List[ExecutionTimePerTimePoint] = timepoints.map(runSingleTimepoint).toList

    val append = StatisticResult.fromMillis(timings.map(_.appendTime))
    val evaluate = StatisticResult.fromMillis(timings.map(_.evaluateTime))

    ExecutionTimePerRun(initializationTime, append, evaluate)
  }


  def runTimepoint(instance: EvaluationInstance, engine: EvaluationEngine, verifyModel: Boolean)(t: Int): ExecutionTimePerTimePoint = {

    val facts = instance.generateFactsToAddAt(t)
    val time = TimePoint(t)

    val appendTime = stopTime {
      engine.append(time)(facts: _*)
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

case class BatchExecutionTimes(runs: List[ExecutionTimePerRun]) {
  val initializationTimes: StatisticResult = StatisticResult.fromMillis(runs.map(_.initializationTime))
  val appendTimes: StatisticResult = StatisticResult.fromExecutionTimes(runs.map(_.appendTime).flatMap(_.executionTimes))
  val evaluateTimes: StatisticResult = StatisticResult.fromExecutionTimes(runs.map(_.evaluateTime).flatMap(_.executionTimes))

  val totalTime: Duration = initializationTimes.total + appendTimes.total + evaluateTimes.total

  val avgTimePerRun: Duration = totalTime / runs.size
}

trait EvaluationInstance {

  val timePoints: Int

  val program: LarsProgram

  def verifyModel(model: Option[Model], t: Int)

  def generateFactsToAddAt(t: Int): Seq[Atom]

  def random: Random
}

trait MMedia {
  val done = Atom("done")
  val lfu = Atom("lfu")
  val lru = Atom("lru")
  val fifo = Atom("fifo")
  val randomAtom = Atom("random")

  val _alpha = Predicate("alpha")
  val high = Atom("high")
  val mid = Atom("mid")
  val low = Atom("low")
  val rtm50 = Atom("rtm50")

  def larsProgram(windowSize: Int): LarsProgram = {

    val T: Variable = StringVariable("T")
    val V: Variable = StringVariable("V")

    def wAt(windowSize: Int, time: Time, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), At(time), atom)
    def wD(windowSize: Int, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), Diamond, atom)
    def wB(windowSize: Int, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), Box, atom)

    def s(ats: Atom*): Set[ExtendedAtom] = ats.toSet

    val n = windowSize

    LarsProgram.from(
      AtAtom(T, high) <= wAt(n, T, _alpha(V)) and Leq(IntValue(18), V),
      AtAtom(T, mid) <= wAt(n, T, _alpha(V)) and Leq(IntValue(12), V) and Lt(V, IntValue(18)),
      AtAtom(T, low) <= wAt(n, T, _alpha(V)) and Lt(V, IntValue(12)),
      lfu <= wB(n, high),
      lru <= wB(n, mid),
      fifo <= wB(n, low) and wD(n, rtm50),
      done <= lfu,
      done <= lru,
      done <= fifo,
      UserDefinedLarsRule(randomAtom, s(), s(done))
    )

  }
}

case class MMediaDeterministic(windowSize: Int, timePoints: Int, random: Random) extends MMedia with EvaluationInstance {

  val program = larsProgram(windowSize)

  def verifyModel(model: Option[Model], t: Int) = {
    val q = t % 180
    if (q >= 0 && q < windowSize) {
      assert(model.contains(randomAtom))
    } else if (q >= windowSize && q < 60) {
      assert(model.contains(randomAtom)) //low, if also rtm holds
    } else if (q >= 60 && q < 60 + windowSize) {
      assert(model.contains(randomAtom))
    } else if (q >= (60 + windowSize) && q < 120) {
      assert(model.contains(lru)) //mid
    } else if (q >= 120 && q < (120 + windowSize)) {
      assert(model.contains(randomAtom))
    } else if (q >= (120 + windowSize) && q < 180) {
      assert(model.contains(lfu)) //high
    }
  }

  def generateFactsToAddAt(t: Int): Seq[Atom] = {
    Seq(_alpha(alphaValueFor(t)))
  }

  def alphaValueFor(t: Int): Int = {
    val q = t % 180
    if (0 <= q && q < 60) 5
    else if (q >= 60 && q < 120) 15
    else 25
  }

}