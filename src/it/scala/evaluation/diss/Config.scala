package evaluation.diss

import evaluation.diss.instances.TestInstance
import evaluation.diss.instances.basic.{BasicDualInstance, BasicInstance}

import scala.util.matching.Regex

/**
  * Created by hb on 05.04.18.
  */
case class Config(var args: Map[String, String]) {

  import Config._

  val instance = args(KEY_INSTANCE)
  val preRuns = Integer.parseInt(args(KEY_PRE_RUNS))
  val runs = Integer.parseInt(args(KEY_RUNS))
  val timePoints = Integer.parseInt(args(KEY_TIMEPOINTS))
  val windowSize = Integer.parseInt(args(KEY_WINDOW_SIZE))
  val withDebug = (args(KEY_WITH_DEBUG) == "true")
  val withHeader = (args(KEY_HEADER) == "true")
  val headerOnly = (args(KEY_HEADER) == "only")
  val reasoner = args(KEY_REASONER)
  val semantics_checks = (args(KEY_SEMANTICS_CHECKS) == "true")
  val verifyModels = (args(KEY_VERIFY_MODELS) == "true")
  val printModelAt = Integer.parseInt(args(KEY_PRINT_MODEL_AT))
  val printRulesAt = Integer.parseInt(args(KEY_PRINT_RULES_AT))
  val simplify = (args(KEY_SIMPLIFY) == "true")

  def makeInstance(iterationNr: Int): Instance = {
    val basic:Regex = """basic_w(t|c)(a|d|b)_([0-9]+)""".r
    val basicDual:Regex = """basic_dual_w(t|c)(a|d|b)_([0-9]+)""".r
    def i(s: String) = Integer.parseInt(s)
    instance match {
      case TEST => TestInstance()
      case basic(winType,mod,signalEvery) => BasicInstance(winType+mod,windowSize,i(signalEvery))
      case basicDual(winType,mod,signalEvery) => BasicDualInstance(winType+mod,windowSize,i(signalEvery))
      case x => throw new RuntimeException("unknown evaluation instance: "+x)
    }
  }

}

object Config {

  def buildArgMap(args: Array[String]): Map[String, String] = {

    if (args.length % 2 == 1) {
      println("need even number of args. given: " + args)
      System.exit(1)
    }
    if (args.length == 0) {
      return Map()
    }

    var argMap = defaultArgs()
    for (i <- 0 to args.length / 2 - 1) {
      argMap = argMap + (args(2 * i) -> args(2 * i + 1))
    }
    argMap

  }

  def defaultArgs(): Map[String, String] = {

    var argMap = Map[String, String]()

    def defaultArg(key: String, value: String) = {
      if (!argMap.contains(key)) {
        argMap = argMap + (key -> value)
      }
    }

    defaultArg(KEY_INSTANCE, "test")
    defaultArg(KEY_REASONER, INCREMENTAL)
    defaultArg(KEY_PRE_RUNS, "2")
    defaultArg(KEY_RUNS, "5")
    defaultArg(KEY_TIMEPOINTS, "100")
    defaultArg(KEY_WINDOW_SIZE, "10")
    //
    defaultArg(KEY_PRINT_RULES, "false")
    defaultArg(KEY_PRINT_RULES_AT, "-1")
    defaultArg(KEY_PRINT_MODEL_AT, "-1")
    defaultArg(KEY_SEMANTICS_CHECKS, "false")
    defaultArg(KEY_VERIFY_MODELS, "true")
    //
    defaultArg(KEY_HEADER, "false")
    defaultArg(KEY_WITH_DEBUG, "false")
    defaultArg(KEY_SIMPLIFY, "false")

    argMap
  }

  val KEY_INSTANCE = "inst"
  val KEY_REASONER = "reasoner"
  val KEY_PRE_RUNS = "pre"
  val KEY_RUNS = "runs"
  val KEY_TIMEPOINTS = "timepoints"
  val KEY_WINDOW_SIZE = "winsize"
  //
  val KEY_SEMANTICS_CHECKS = "checks"
  val KEY_VERIFY_MODELS = "verify"
  //
  val KEY_HEADER = "header"
  val KEY_WITH_DEBUG = "withDebug"
  val KEY_PRINT_RULES = "printRules"
  val KEY_PRINT_RULES_AT = "printRulesAt"
  val KEY_PRINT_MODEL_AT = "printModelAt"
  //
  val KEY_SIMPLIFY = "simplify"

  // REASONERS
  val CLINGO = "clingo"
  val INCREMENTAL = "incr"

  // INSTANCES
  val TEST = "test"

}