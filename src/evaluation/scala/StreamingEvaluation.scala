import java.io.File
import java.util.concurrent.TimeUnit

import common.Util.stopTime
import Program.InputTypes.InputTypes
import StreamingEvaluation.EvaluationTypes.Type
import StreamingEvaluation.Instance.Type
import common.{Plot, Util}
import core.lars._
import core._
import core.asp.{AspFact, NormalRule}
import engine.{EvaluationEngine, Result}
import engine.asp.tms.policies.LazyRemovePolicy
import engine.config.{BuildEngine, EvaluationModifier, EvaluationTypes, StartableEngineConfiguration}
import engine.config.EvaluationModifier.{EvaluationModifier, Value}
import engine.config.EvaluationTypes.{EvaluationTypes, Value}
import evaluation._
import jtms.JtmsUpdateAlgorithm
import jtms.algorithms.JtmsDoyleHeuristics
import jtms.networks.OptimizedNetwork

import scala.collection.immutable.HashMap
import scala.concurrent.duration.Duration
import scala.util.Random

/**
  * Created by fm on 24/02/2017.
  */
object StreamingEvaluation {

  def main(args: Array[String]): Unit = {

    val myargs = Seq(
      "--runs", "4"
    ).toArray

    
    timings(args)
  }

  def timings(args: Array[String]): Unit = {
    if (args.length == 0) {
      evaluate(Config(
        instance = Instance.MMedia_Det,
        preRuns = 3,
        runs = 5,
        timePoints = 100,
        windowSize = 10))
    } else {
      parseParameters(args) match {
        case Some(config) => evaluate(config)
        case None => println("could not parse arguments")
      }
    }
  }

  def evaluate(config: Config) = {
    val configWithParam = Util.prettyPrint(config)

    // we want '#' before each line
    if (config.withDebug)
      println(configWithParam.linesWithSeparators.map(l => f"# $l").mkString)


    val executionTimes = BatchExecutionTimes(run(config))

    val outputValues = Map(
      "total_time" -> executionTimes.avgTimePerRun,
      "init_time" -> executionTimes.initializationTimes.avg,
      "add_time" -> executionTimes.appendTimes.avg,
      "eval_time" -> executionTimes.evaluateTimes.avg
    )

    // TODO: can we use .keys / .values? - Not sure if they keep the order
    val separator = ";"
    if (config.withHeader) {
      print("# config" + separator)
      println(outputValues.map(_._1).mkString(separator))
    }

    val values = outputValues.collect {
      case (_, d: Duration) => d.toMillis
    }

    print(config.title.getOrElse(f"CFG: ${config.windowSize} ${config.runs}"))
    println(values.mkString(separator))
  }

  def run(config: Config): List[ExecutionTimePerRun] = {

    val instance = config.buildInstance()

    val evaluateRuns = evaluateRun(config, instance) _

    def generateRandom(seedHint: Int) = config.randomSeed match {
      case None => new Random(seedHint)
      case Some(seed) => new Random(seed)
    }

    (-config.preRuns until 0).
      foreach { i =>
        if (config.withDebug)
          println(f"# pre-run $i")
        evaluateRuns(generateRandom(i))
      }

    val runResults = (0 to config.runs).
      map { i =>
        if (config.withDebug)
          println(f"# run $i")
        evaluateRuns(generateRandom(i))
      }.
      toList

    runResults
  }


  def evaluateRun(config: Config, instance: EvaluationInstance)(random: Random): ExecutionTimePerRun = {

    val builder = BuildEngine.withProgram(instance.program)

    var engine: EvaluationEngine = null

    val initializationTime = stopTime {

      val startableEngine = config.evaluationType match {
        case EvaluationTypes.ClingoPush => builder.configure().withClingo().use().usePush()
        case EvaluationTypes.DoyleHeuristics =>
          val tms = new JtmsDoyleHeuristics(new OptimizedNetwork(), random)

          if (config.semanticCheck) {
            tms.doConsistencyCheck = true
            tms.doJtmsSemanticsCheck = true
            tms.doSelfSupportCheck = true
          }

          builder.configure().
            withTms().
            withPolicy(LazyRemovePolicy(tms)).
            withIncremental()
      }

      engine = startableEngine.start()

    }

    val timepoints = Seq.range(0, config.timePoints)

    val runTimepoints = runTimepoint(instance, engine, config.verifyModel) _

    val timings = timepoints.map(runTimepoints).toList

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

    if (verifyModel)
      instance.verifyModel(result.get, t)

    ExecutionTimePerTimePoint(time, appendTime, evaluateTime)
  }

  object EvaluationTypes extends Enumeration {
    type Type = Value
    val DoyleHeuristics, ClingoPush = Value
  }


  object Instance extends Enumeration {
    type Type = Value
    val MMedia_Det, MMedia_NonDet, Cache_Hops_1, Cache_Hops_2 = Value
  }

  case class Config(evaluationType: EvaluationTypes.Type = EvaluationTypes.DoyleHeuristics,
                    preRuns: Int = 1,
                    runs: Int = 1,
                    timePoints: Int = 2,
                    windowSize: Int = 2,
                    instance: Instance.Type = Instance.MMedia_Det,
                    items: Int = 1,
                    semanticCheck: Boolean = false,
                    verifyModel: Boolean = false,
                    randomSeed: Option[Long] = None,
                    withHeader: Boolean = true,
                    title: Option[String] = None,
                    withDebug: Boolean = true
                   ) {

    def buildInstance(): EvaluationInstance = instance match {
      case Instance.MMedia_Det => MMediaDeterministic(windowSize)
    }
  }

  def parseParameters(args: Array[String]) = {
    implicit val evaluationTypesRead: scopt.Read[EvaluationTypes.Type] =
      scopt.Read.reads(EvaluationTypes withName)

    implicit val instanceRead: scopt.Read[Instance.Type] =
      scopt.Read.reads(Instance withName)

    val parser = new scopt.OptionParser[Config]("scopt") {
      head("scopt", "3.x")

      opt[EvaluationTypes.Type]('e', "evaluationType").optional().valueName("<evaluation type>").
        action((x, c) => c.copy(evaluationType = x)).
        text("An evaluation type is required, possible values: " + EvaluationTypes.values)

      opt[Instance.Type]('i', "instance").optional().valueName("<instance identifier>").
        action((x, c) => c.copy(instance = x)).
        text("An instance identifier is required, possible values: " + Instance.values)

      opt[Int]('p', "pre").optional().valueName("<value>").
        action((x, c) => c.copy(preRuns = x))

      opt[Int]('r', "runs").optional().valueName("<value>").
        action((x, c) => c.copy(runs = x))

      opt[Int]('t', "tp").optional().valueName("<value>").
        action((x, c) => c.copy(timePoints = x))

      opt[Int]('w', "winsize").optional().valueName("<value>").
        action((x, c) => c.copy(windowSize = x))

      opt[Int]('i', "items").optional().valueName("<value>").
        action((x, c) => c.copy(items = x))

      opt[Boolean]('s', "checks").optional().valueName("<value>").
        action((x, c) => c.copy(semanticCheck = x))

      opt[Boolean]('v', "verify").optional().valueName("<value>").
        action((x, c) => c.copy(verifyModel = x))

      opt[Long]("randomSeed").optional().
        action((x, c) => c.copy(randomSeed = Some(x)))

      opt[String]("title").optional().
        action((x, c) => c.copy(title = Some(x)))

      opt[Boolean]('d', "debug").optional().valueName("<value>").
        action((x, c) => c.copy(withDebug = x))


      help("help").text("Specify init parameters for running the evaluation")
    }

    parser.parse(args, Config())
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
  val program: LarsProgram

  def verifyModel(model: Option[Model], t: Int)

  def generateFactsToAddAt(t: Int): Seq[Atom]
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

case class MMediaDeterministic(windowSize: Int) extends MMedia with EvaluationInstance {

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