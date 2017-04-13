package jtms.evaluation

import java.io.{File, PrintWriter}
import java.util.concurrent.TimeUnit

import common.Util.stopTime
import core.asp._
import jtms._
import jtms.algorithms._
import jtms.evaluation.instances.{CacheHopsEvalInst1, CacheHopsEvalInst2, MMediaDeterministicEvalInst, MMediaNonDeterministicEvalInst}
import jtms.networks.{OptimizedNetwork, OptimizedNetworkForLearn, SimpleNetwork}
import runner.Load

import scala.io.Source
import scala.util.Random


/**
  * Created by hb on 4/4/17.
  */
object StreamingTmsEval {

  //known instances:
  val MMEDIA_DET = "mmediaDet"
  val MMEDIA_NONDET = "mmediaNonDet"
  val CACHE_HOPS1 = "cacheHops1"
  val CACHE_HOPS2 = "cacheHops2"

  val loader = Load(TimeUnit.SECONDS)

  def main(args: Array[String]): Unit = {
    evaluate(args)
  }

  //implementations:
  val DOYLE_SIMPLE = "DoyleSimple"
  val DOYLE = "Doyle"
  val DOYLE_HEURISTICS = "DoyleHeur"
  val GREEDY = "Greedy"
  val LEARN = "Learn"

  var INSTANCE_NAME = "inst"
  var TMS = "tms"
  var PRE_RUNS = "pre"
  var RUNS = "runs"
  var TIMEPOINTS = "tp"
  var MODEL_RATIO = "ratio"
  var WINDOW_SIZE = "winsize"
  //
  var ITEMS = "items"
  //
  var POST_PROCESS_GROUNDING = "postProcess"
  var PRINT_RULES = "printRules"
  var INDICATE_TIMEPOINTS = "dots"
  var SEMANTICS_CHECKS = "checks"

  var argMap = Map[String,String]() //make accessible globally for faster dev access

  def evaluate(args: Array[String]): Unit = {

    argMap = buildArgMap(args)

    def defaultArg(key: String, value: String) = {
      if (!argMap.contains(key)) {
        argMap = argMap + (key -> value)
      }
    }

    defaultArg(INSTANCE_NAME,CACHE_HOPS2)
    defaultArg(TMS,DOYLE_HEURISTICS)
    defaultArg(PRE_RUNS,"0")
    defaultArg(RUNS,"1")
    defaultArg(TIMEPOINTS,"2")
    defaultArg(MODEL_RATIO,"false")
    defaultArg(WINDOW_SIZE,"2")
    //
    defaultArg(ITEMS,"1")
    //
    defaultArg(POST_PROCESS_GROUNDING,"true")
    defaultArg(PRINT_RULES,"false")
    defaultArg(INDICATE_TIMEPOINTS,"false")
    defaultArg(SEMANTICS_CHECKS,"false")

    run(Config(argMap))

  }

  def buildArgMap(args: Array[String]): Map[String,String] = {

    if (args.length % 2 == 1) {
      println("need even number of args. given: "+args)
      System.exit(1)
    }
    if (args.length == 0) {
      return Map()
    }

    var argMap = Map[String,String]()
    for (i <- 0 to args.length/2-1) {
      argMap = argMap + (args(2*i) -> args(2*i+1))
    }
    argMap

  }

  case class Config(args: Map[String,String]) {

    val preRuns = Integer.parseInt(argMap(PRE_RUNS))
    val runs = Integer.parseInt(argMap(RUNS))
    val modelRatio:Boolean = (argMap(MODEL_RATIO) == "true")
    val timePoints = Integer.parseInt(argMap(TIMEPOINTS))
    val windowSize = Integer.parseInt(argMap(WINDOW_SIZE))
    val nrOfItems = Integer.parseInt(argMap(ITEMS))
    val postProcessGrounding = (argMap(POST_PROCESS_GROUNDING) == "true")

    def makeInstance(iterationNr: Int): StreamingTmsEvalInst = {
      val random = new Random(iterationNr)
      argMap(INSTANCE_NAME) match {
        case CACHE_HOPS1 => {
          val printRules: Boolean = (argMap(PRINT_RULES) == "true")
          CacheHopsEvalInst1(windowSize,timePoints,nrOfItems,postProcessGrounding,printRules,random)
        }
        case CACHE_HOPS2 => {
          val printRules: Boolean = (argMap(PRINT_RULES) == "true")
          CacheHopsEvalInst2(windowSize,timePoints,nrOfItems,postProcessGrounding,printRules,random)
        }
        case MMEDIA_DET => MMediaDeterministicEvalInst(windowSize, timePoints, random)
        case MMEDIA_NONDET => MMediaNonDeterministicEvalInst(windowSize, timePoints, random)
        case s => println(f"unknown instance name $s"); throw new RuntimeException
      }
    }

    def makeTms(inst: StreamingTmsEvalInst): JtmsUpdateAlgorithm = {
      val tms = argMap(TMS) match {
        case DOYLE_SIMPLE => new JtmsDoyle(new SimpleNetwork(), inst.random)
        case DOYLE => new JtmsDoyle(new OptimizedNetwork(), inst.random)
        case DOYLE_HEURISTICS => new JtmsDoyleHeuristics(new OptimizedNetwork(), inst.random)
        case GREEDY => new JtmsGreedy(new OptimizedNetwork(), inst.random)
        case LEARN => new JtmsLearn(new OptimizedNetworkForLearn(), inst.random)
      }

      if ((tms.isInstanceOf[JtmsDoyle]) && argMap(SEMANTICS_CHECKS) == "true") {
        tms.asInstanceOf[JtmsDoyle].doConsistencyCheck=true
        tms.asInstanceOf[JtmsDoyle].doJtmsSemanticsCheck=true
        tms.asInstanceOf[JtmsDoyle].doSelfSupportCheck=true
      }

      tms
    }

    def isSomeDoyle() = {
      argMap(TMS).toLowerCase().contains("doyle")
    }

  }

  def run(cfg: Config): Unit = {

    println("run with args: "+cfg.args)

    var totalTime = 0L
    var totalRetractions = 0L
    var totalModels = 0L
    var totalFailures = 0L
    var totalTimeRuleGen = 0L

    var totalTimeStaticRules = 0L
    var totalTimeAllTimePoints = 0L
    var totalTimeAddFact = 0L
    var totalTimeAddRule = 0L
    var totalTimeRemoveRule = 0L
    var totalTimeRemoveFact = 0L
    var totalTimeGetModel = 0L
    var totalNrStaticRules = 0L
    var totalNrAddFact = 0L
    var totalNrAddRule = 0L
    var totalNrRemoveRule = 0L
    var totalNrRemoveFact = 0L

    for (i <- (1 + (cfg.preRuns * -1)) to cfg.runs) {

      print(" " + i)

      val instance: StreamingTmsEvalInst = cfg.makeInstance(i) //init only here for having different random seeds

      val result: Map[String, Long] = runIteration(instance,cfg)

      if (i >= 1) {
        totalTime += result(_evaluationIterationTime)
        totalModels += result(_models)
        totalFailures += result(_failures)
        totalTimeRuleGen += result(_timeRuleGen)
        totalTimeAllTimePoints += result(_timeAllTimePoints)
        totalTimeStaticRules += result(_timeStaticRules)
        totalTimeAddFact += result(_timeAddFacts)
        totalTimeAddRule += result(_timeAddRules)
        totalTimeRemoveRule += result(_timeRemoveRules)
        totalTimeRemoveFact += result(_timeRemoveFacts)
        totalTimeGetModel += result(_timeGetModel)
        totalNrStaticRules += result(_nrOfStaticRules)
        totalNrAddFact += result(_nrOfAddedFacts)
        totalNrAddRule += result(_nrOfAddedRules)
        totalNrRemoveRule += result(_nrOfRemovedRules)
        totalNrRemoveFact += result(_nrOfRemovedFacts)
      }

      if (cfg.isSomeDoyle()) {
        totalRetractions += result(_nrOfRetractionsAffected)
      }

    }

    case class LongDiv(l: Long) {
      def %% (other: Long): Double = (1.0*l) / (1.0*other)
    }

    case class DoubleSec(d: Double) {
      def sec(): Double = d / 1000.0
    }

    implicit def long2div(l: Long) = LongDiv(l)
    implicit def double2sec(d: Double) = DoubleSec(d)

    val runs = cfg.runs
    val tp = cfg.timePoints

    val avgTimeIteration = totalTime %% runs sec
    val avgTimeRuleGen = totalTimeRuleGen %% runs sec
    val avgTimeStaticRules = totalTimeStaticRules %% runs sec
    val avgTimeAllTimePoints = totalTimeAllTimePoints %% (runs * tp) sec
    val avgTimeAddFact = totalTimeAddFact %% totalNrAddFact sec
    val avgTimeAddRule = totalTimeAddRule %% totalNrAddRule sec
    val avgTimeRemoveRule = totalTimeRemoveRule %% totalNrRemoveRule sec
    val avgTimeRemoveFact = totalTimeRemoveFact %% totalNrRemoveFact sec
    val avgTimeGetModel = totalTimeGetModel %% (runs * tp) sec
    val totalUpdates = totalModels + totalFailures
    val ratioModels = totalModels %% totalUpdates
    val ratioFailures = totalFailures %% totalUpdates

    println(f"\niteration avg:")
    println(f"total time: $avgTimeIteration sec")
    println(f"rule generation (not included): $avgTimeRuleGen sec")
    println(f"add static rules: $avgTimeStaticRules sec")
    println(f"avg per time point: $avgTimeAllTimePoints sec")
    //println(f"avg time to add fact: $avgTimeAddFact sec")
    //println(f"avg time to add rule: $avgTimeAddRule sec")
    //println(f"avg time to remove rule: $avgTimeRemoveRule sec")
    //println(f"avg time to remove fact: $avgTimeRemoveFact sec")
    //println(f"avg time to get model: $avgTimeGetModel sec")
    println(f"ratio models: $ratioModels")
    //println(f"ratio failures: $ratioFailures")

    if (cfg.isSomeDoyle()) {
      val avgRetractions = (1.0 * totalRetractions) / (1.0 * runs)
      println(f"avg retractions: $avgRetractions")
    }

  }

  val _evaluationIterationTime = "evaluationIterationTime"
  val _models = "models"
  val _failures = "failures"
  val _timeRuleGen = "timeRuleGen" //only internal info
  val _timeStaticRules = "timeStaticRules"
  val _timeAllTimePoints = "timeAllTimePoints"
  val _timeAddFacts = "timeAddFact"
  val _timeAddRules = "timeAddRule"
  val _timeRemoveRules = "timeRemoveRule"
  val _timeRemoveFacts = "timeRemoveFact"
  val _timeGetModel = "timeGetModel"
  val _nrOfStaticRules = "nrOfInitRules"
  val _nrOfAddedFacts = "nrOfAddFact"
  val _nrOfAddedRules = "nrOfAddRule"
  val _nrOfRemovedRules = "nrOfRemoveRule"
  val _nrOfRemovedFacts = "nrOfRemoveFact"
  val _nrOfRetractionsAffected = "nrOfRetractionsAffected"

  def runIteration(inst: StreamingTmsEvalInst, cfg: Config): Map[String, Long] = {

    val tms = cfg.makeTms(inst)

    var models = 0L
    var failures = 0L

    val timeStaticRules: Long = stopTime {
      inst.staticRules foreach tms.add
    }
    val nrOfStaticRules: Long = inst.staticRules.size

    var timeAllTimePoints = 0L
    var timeRuleGen = 0L
    var timeAddFacts = 0L
    var timeAddRules = 0L
    var timeRemoveRules = 0L
    var timeRemoveFacts = 0L
    var timeGetModel = 0L
    var nrOfAddedFacts = 0L
    var nrOfAddedRules = 0L
    var nrOfRemovedRules = 0L
    var nrOfRemovedFacts = 0L
    var nrOfRetractionsAffected = 0L

    for (t <- 0 to inst.timePoints) {

      if (argMap(INDICATE_TIMEPOINTS) == "true") print(".")

      var factsToAdd = Seq[NormalRule]()
      var rulesToAdd = Seq[NormalRule]()
      var rulesToRemove = Seq[NormalRule]()
      var factsToRemove = Seq[NormalRule]()

      timeRuleGen = timeRuleGen + stopTime {
        factsToAdd = inst.factsToAddAt(t)
        rulesToAdd = inst.rulesToAddAt(t)
        rulesToRemove = inst.rulesToRemoveAt(t)
        factsToRemove = inst.factsToRemoveAt(t)
        nrOfAddedFacts += factsToAdd.size
        nrOfAddedRules += rulesToAdd.size
        nrOfRemovedRules += rulesToRemove.size
        nrOfRemovedFacts += factsToRemove.size
      }

      var loopTimeAddFacts = 0L
      var loopTimeAddRules = 0L
      var loopTimeRemoveRules = 0L
      var loopTimeRemoveFacts = 0L
      var loopTimeGetModel = 0L

      factsToAdd foreach { r =>
        //println("add "+r)
        loopTimeAddFacts += stopTime { tms.add(r) }
        if (cfg.modelRatio) {
          if (tms.getModel.isDefined) models += 1
          else failures += 1
        }
      }

      rulesToAdd foreach { r =>
        //println("add "+r)
        loopTimeAddRules += stopTime { tms.add(r) }
        if (cfg.modelRatio) {
          if (tms.getModel.isDefined) models += 1
          else failures += 1
        }
      }

      rulesToRemove foreach { r =>
        //println("remove "+r)
        loopTimeRemoveRules += stopTime { tms.remove(r) }
        if (cfg.modelRatio) {
          if (tms.getModel.isDefined) models += 1
          else failures += 1
        }
      }

      factsToRemove foreach { r =>
        //println("remove "+r)
        loopTimeRemoveFacts += stopTime { tms.remove(r) }
        if (cfg.modelRatio) {
          if (tms.getModel.isDefined) models += 1
          else failures += 1
        }
      }

      loopTimeGetModel += stopTime { tms.getModel }

      val loopTime = loopTimeAddFacts + loopTimeAddRules + loopTimeRemoveRules + loopTimeRemoveFacts + loopTimeGetModel

      timeAllTimePoints += loopTime
      timeAddFacts += loopTimeAddFacts
      timeAddRules += loopTimeAddRules
      timeRemoveRules += loopTimeRemoveRules
      timeRemoveFacts += loopTimeRemoveFacts
      timeGetModel += loopTimeGetModel

      inst.verifyModel(tms,t)

    }

    val evaluationIterationTime = timeStaticRules + timeAllTimePoints

    /*
    if (tms.isInstanceOf[JtmsDoyle]) {
      val jtms = tms.asInstanceOf[JtmsDoyle]
      jtms.doConsistencyCheck = true
      jtms.doJtmsSemanticsCheck = true
      jtms.doSelfSupportCheck = true
      jtms.checkConsistency()
      jtms.checkJtmsSemantics()
      jtms.checkSelfSupport()
    }
    */
    if (cfg.isSomeDoyle()) {
      nrOfRetractionsAffected = tms.asInstanceOf[JtmsDoyle].retractionsAffected
    }

    Map() + (_evaluationIterationTime -> evaluationIterationTime) + (_models -> models) + (_failures -> failures) + (_timeRuleGen -> timeRuleGen) +
        (_timeStaticRules -> timeStaticRules) + (_timeAllTimePoints -> timeAllTimePoints) +
        (_timeAddFacts -> timeAddFacts) + (_timeAddRules -> timeAddRules) +
        (_timeRemoveRules -> timeRemoveRules) + (_timeRemoveFacts -> timeRemoveFacts) + (_timeGetModel -> timeGetModel) +
        (_nrOfStaticRules -> nrOfStaticRules) + (_nrOfAddedFacts -> nrOfAddedFacts) + (_nrOfAddedRules -> nrOfAddedRules) +
        (_nrOfRemovedRules -> nrOfRemovedRules) + (_nrOfRemovedFacts -> nrOfRemovedFacts) + (_nrOfRetractionsAffected -> nrOfRetractionsAffected)

  }

  def readProgramFromFile(filename: String): NormalProgram = {
    //val source = Source.fromURL(getClass.getResource(filename))
    val source = Source.fromFile(new File(filename))
    val rules = source.getLines().toSeq map (l => Util.asAspRule(loader.rule(l)))
    AspProgram(rules.toList)
  }

  def writeProgramToFile(program: NormalProgram, filename: String) = {
    val pw = new PrintWriter(new File(filename))
    program.rules foreach (r => pw.write(r.toString + "\n"))
    pw.close
  }

  def printModel(tms: JtmsUpdateAlgorithm): Unit = {
    println("model for " + tms.getClass.getSimpleName)
    tms.getModel match {
      case Some(m) => println(m); println("#atoms: " + m.size)
      case None => println("none")
    }
  }

  /*
  test("infinite odd loop doyle") {

    val tms = new JtmsDoyle()
    val r1 = asAspRule(rule("a :- not b"))
    val r2 = asAspRule(rule("b :- a"))
    tms add r1
    tms add r2

    println(tms.getModel)
  }
  */

}
