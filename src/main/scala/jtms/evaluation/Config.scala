package jtms.evaluation

import jtms.JtmsUpdateAlgorithm
import jtms.algorithms.{JtmsDoyle, JtmsDoyleHeuristics, JtmsGreedy, JtmsLearn}
import jtms.evaluation.instances._
import jtms.networks.{OptimizedNetwork, OptimizedNetworkForLearn, SimpleNetwork}

import scala.util.Random

/**
  * Created by hb on 17.04.17.
  */
case class Config(var args: Map[String,String]) {

  import Config._

  val instanceName = args(INSTANCE_NAME)
  val preRuns = Integer.parseInt(args(PRE_RUNS))
  val runs = Integer.parseInt(args(RUNS))
  val modelRatio:Boolean = (args(MODEL_RATIO) == "true")
  val timePoints = Integer.parseInt(args(TIMEPOINTS))
  val inputWindowSize = Integer.parseInt(args(WINDOW_SIZE))
  val signalsPerTp = Integer.parseInt(args(SIGNALS_PER_TP))
  val nrOfItems = Integer.parseInt(args(ITEMS))
  val withDebug = (args(WITH_DEBUG) == "true")
  val withHeader = (args(HEADER) == "true")
  val headerOnly = (args(HEADER) == "only")
  val implementation = args(IMPL)
  val verifyModel = (args(VERIFY_MODEL) == "true")
  val printRulesAt = Integer.parseInt(args(PRINT_RULES_AT))

  var windowSize = if (inputWindowSize == -1) 100 else inputWindowSize

  def makeInstance(iterationNr: Int): StreamingTmsEvalInst = {
    val random = new Random(iterationNr)
    args(INSTANCE_NAME) match {
      case CACHE_HOPS1 => {
        if (verifyModel) {
          windowSize = 10
        }
        CacheHopsEvalInst1(windowSize,timePoints,nrOfItems,random)
      }
      case CACHE_HOPS2 => {
        if (verifyModel) {
          windowSize = 15
        }
        CacheHopsEvalInst2(windowSize,timePoints,nrOfItems,random)
      }
      case CACHE_HOPS3 => {
        CacheHopsEvalInst3(windowSize,timePoints,nrOfItems,signalsPerTp,random) //window size is fixed to 15 (for verification)
      }
      case MMEDIA_DET => {
        MMediaDeterministicEvalInst(windowSize, timePoints, random)
      }
      case MMEDIA_NONDET => {
        MMediaNonDeterministicEvalInst(windowSize, timePoints, random)
      }
      case s => println(f"unknown instance name $s"); throw new RuntimeException
    }
  }

  def makeTms(inst: StreamingTmsEvalInst): JtmsUpdateAlgorithm = {
    val tms = args(IMPL) match {
      case DOYLE_SIMPLE => new JtmsDoyle(new SimpleNetwork(), inst.random)
      case DOYLE => new JtmsDoyle(new OptimizedNetwork(), inst.random)
      case DOYLE_HEURISTICS => new JtmsDoyleHeuristics(new OptimizedNetwork(), inst.random)
      case GREEDY => new JtmsGreedy(new OptimizedNetwork(), inst.random)
      case LEARN => new JtmsLearn(new OptimizedNetworkForLearn(), inst.random)
      case _ => throw new RuntimeException("unknown tms impl: "+args(IMPL))
    }

    if (tms.isInstanceOf[JtmsDoyle] && (args(SEMANTICS_CHECKS) == "true")) {
      tms.asInstanceOf[JtmsDoyle].doConsistencyCheck=true
      tms.asInstanceOf[JtmsDoyle].doJtmsSemanticsCheck=true
      tms.asInstanceOf[JtmsDoyle].doSelfSupportCheck=true
    }

    tms
  }

  def isSomeDoyle() = {
    args(IMPL).toLowerCase().contains("doyle")
  }

}

object Config {

  //known instances:
  val MMEDIA_DET = "mmediaDet"
  val MMEDIA_NONDET = "mmediaNonDet"
  val CACHE_HOPS1 = "cacheHops1"
  val CACHE_HOPS2 = "cacheHops2"
  val CACHE_HOPS3 = "cacheHops3"

  //implementations:
  val DOYLE_SIMPLE = "DoyleSimple"
  val DOYLE = "Doyle"
  val DOYLE_HEURISTICS = "DoyleHeur"
  val GREEDY = "Greedy"
  val LEARN = "Learn"
  val CLINGO_PUSH = "ClingoPush"
  val CLINGO_PULL = "ClingoPull"

  val INSTANCE_NAME = "inst"
  val IMPL = "impl"
  val PRE_RUNS = "pre"
  val RUNS = "runs"
  val TIMEPOINTS = "tp"
  val MODEL_RATIO = "ratio"
  val WINDOW_SIZE = "winsize"
  val SIGNALS_PER_TP = "signalsPerTp"
  //
  val ITEMS = "items" //* (for cache hops)
  //
  val SEMANTICS_CHECKS = "checks"
  val VERIFY_MODEL = "verify"
  //
  val HEADER = "header"
  val WITH_DEBUG = "withDebug"
  val PRINT_RULES = "printRules"
  val PRINT_RULES_AT = "printRulesAt"

  def buildArgMap(args: Array[String]): Map[String,String] = {

    if (args.length % 2 == 1) {
      println("need even number of args. given: "+args)
      System.exit(1)
    }
    if (args.length == 0) {
      return Map()
    }

    var argMap = defaultArgs()
    for (i <- 0 to args.length/2-1) {
      argMap = argMap + (args(2*i) -> args(2*i+1))
    }
    argMap

  }

  def defaultArgs(): Map[String,String] = {

    var argMap = Map[String,String]()

    def defaultArg(key: String, value: String) = {
      if (!argMap.contains(key)) {
        argMap = argMap + (key -> value)
      }
    }

    defaultArg(INSTANCE_NAME,CACHE_HOPS2)
    defaultArg(IMPL,DOYLE_HEURISTICS)
    defaultArg(PRE_RUNS,"2")
    defaultArg(RUNS,"5")
    defaultArg(TIMEPOINTS,"20")
    defaultArg(MODEL_RATIO,"false")
    defaultArg(WINDOW_SIZE,"-1")
    defaultArg(SIGNALS_PER_TP,"1")
    //
    defaultArg(ITEMS,"1")
    //
    defaultArg(PRINT_RULES, "false")
    defaultArg(PRINT_RULES_AT, "-1")
    defaultArg(SEMANTICS_CHECKS, "false")
    defaultArg(VERIFY_MODEL, "true")
    //
    defaultArg(HEADER, "false")
    defaultArg(WITH_DEBUG, "false")

    argMap
  }


}