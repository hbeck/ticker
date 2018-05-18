package evaluation.diss

import evaluation.diss.instances._
import evaluation.diss.instances.analytic._
import evaluation.diss.instances.traits.Instance

import scala.util.Random
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
  val profiling = (args(KEY_PROFILING) == "true")
  val overrideRandom = (!args(KEY_RANDOM).isEmpty)
  val fixedRandom = if (overrideRandom) Integer.parseInt(args(KEY_RANDOM)) else 0
  val prevModelHeuristic = (args(KEY_PREVMODEL_HEURISTIC) == "true")
  val updateSuppRuleHeuristic = (args(KEY_UPDATE_SUPPRULE_HEURISTIC) == "true")

  def makeInstance(iterationNr: Int): Instance = {

    val random: Random = new Random(iterationNr)

    val srBasic:Regex = """srbasic_w(t|c)(a|d|b)_n([0-9]+)_p(0?|1)\.([0-9]*)""".r //eg srbasic_wtd_n1000_p0.9 //*
    val reachSig:Regex = """rs_w(t|c)(a|d|b)_n([0-9]+)_p(0?|1)\.([0-9]*)""".r //eg rs_wtd_n100_p0.9 //*
    val strat:Regex = """strat_n([0-9]+)_p(0?|1)\.([0-9]*)""".r //eg strat_n30_p0.1 //*
    //
    val basic:Regex = """basic_w(t|c)(a|d|b)_([0-9]+)""".r //eg basic_wtd_1
    val nBasic:Regex = """nbasic_w(t|c)(a|d|b)_([0-9]+)""".r //eg nbasic_wtd_1
    val basicDual:Regex = """basic_dual_w(t|c)(a|d|b)_([0-9]+)""".r //eg basic_dual_wtd_1
    val sdBasic:Regex = """sdbasic_w(t|c)(a|d|b)_n([0-9]+)_e([0-9]+)""".r //eg sdbasic_wtd_n1000_e1
    val join:Regex = """join_w(t|c)(a|d|b)_([0-9]+)_([0-9]+)""".r //eg join_wtd_1_10
    val reach:Regex = """reach_w(t|c)(a|d|b)_([0-9]+)_([0-9]+)""".r //eg reach_wtd_1_10
    val reachLt:Regex = """reach_lt_w(t|c)(a|d|b)_([0-9]+)_([0-9]+)""".r //eg reach_lt_wtd_1_10
    val reachPerc:Regex = """rp_w(t|c)(a|d|b)_e([0-9]+)_n([0-9]+)_p([0-9]+)""".r //eg rp_lt_wtd_e10_n100_p30
    val linReachPerc:Regex = """lrp_w(t|c)(a|d|b)_e([0-9]+)_n([0-9]+)_p([0-9]+)""".r //eg lrp_wtd_e10_n100_p30
    val reachSigDual:Regex = """rsd_w(t|c)(a|d|b)_n([0-9]+)_p(0?|1)\.([0-9]*)""".r //eg rsd_wtb_n100_p0.9
    val tme:Regex = """tme_w(t|c)(a|d|b)_n([0-9]+)_a(0?|1)\.([0-9]*)""".r //eg tme_wtd_n100_a0.9
    val carsDet:Regex = """carsdet_n([0-9]+)_k([0-9]+)""".r //eg carsdet_n100_k10

    instance match {
      case SAMPLE => SampleInstance()
      case srBasic(winType,mod,scale,probL,probR) => ScalableRandomizedBasicInstance(random,winType+mod, windowSize, i(scale), d(probL,probR))
      case reachSig(winType,mod,scale,sigL,sigR) => {
        ReachSigInstance(random,winType+mod,windowSize,i(scale),d(sigL,sigR))
      }
      case strat(scale,pL,pR) => {
        StrategyInstance(random,windowSize,i(scale),d(pL,pR))
      }
      //
      case basic(winType,mod,signalEvery) => BasicInstance(winType+mod,windowSize,i(signalEvery))
      case nBasic(winType,mod,signalEvery) => NBasicInstance(winType+mod,windowSize,i(signalEvery))
      case basicDual(winType,mod,signalEvery) => BasicDualInstance(winType+mod,windowSize,i(signalEvery))
      case join(winType,mod,signalEvery,scale) => JoinInstance(winType+mod,windowSize,i(signalEvery),i(scale))
      case sdBasic(winType,mod,scale,allSignalsEvery) => ScalableDeterministicBasicInstance(winType+mod, windowSize, i(scale), i(allSignalsEvery))
      case reach(winType,mod,signalEvery,scale) => ReachInstance(random,winType+mod,windowSize,i(signalEvery),i(scale))
      case reachLt(winType,mod,signalEvery,scale) => ReachLtInstance(random,winType+mod,windowSize,i(signalEvery),i(scale))
      case reachPerc(winType,mod,signalEvery,scale,percent) => ReachPercInstance(random,winType+mod,windowSize,i(signalEvery),i(scale),i(percent))
      case linReachPerc(winType,mod,signalEvery,scale,percent) => LinReachPercInstance(random,winType+mod,windowSize,i(signalEvery),i(scale),i(percent))
      case reachSigDual(winType,mod,scale,sigL,sigR) => {
        ReachSigDualInstance(random,winType+mod,windowSize,i(scale),d(sigL,sigR))
      }
      case tme(winType,mod,scale,signalProbL,signalProbR) => TwoModelsEasyInstance(random,winType+mod,windowSize,i(scale),d(signalProbL,signalProbR))
      case carsDet(scale,k) => CarsDeterministicInstance(i(scale), windowSize, i(k))
      case x => throw new RuntimeException(f"unknown evaluation instance: $x")
    }

  }

  def i(s: String): Int = Integer.parseInt(s)
  //("0","98") or ("","98") ==> 0.98. args: left and right of dot
  def d(left: String, right: String): Double = {
    val l = if (left.isEmpty) 0 else Integer.parseInt(left)
    val r = if (right.isEmpty) 0 else Integer.parseInt(right)
    if (l==0) {
      (1.0*r)/(1.0*Math.pow(10,right.length))
    } else {
      if (r != 0) { throw new RuntimeException(f"unknown probability $left.$right") }
      1.0
    }
  }

}

object Config {

  def buildArgMap(args: Array[String]): Map[String, String] = {

    if (args.length % 2 == 1) {
      print(f"need even number of args. given ${args.length}: ")
      args.foreach(a => print(f" $a"))
      println()
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
    defaultArg(KEY_PRE_RUNS, "0")
    defaultArg(KEY_RUNS, "1")
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
    //
    defaultArg(KEY_PROFILING, "false")
    //
    defaultArg(KEY_RANDOM, "")
    //
    defaultArg(KEY_UPDATE_SUPPRULE_HEURISTIC, "true")
    defaultArg(KEY_PREVMODEL_HEURISTIC, "false")

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
  val KEY_RANDOM = "rand"
  //
  val KEY_SIMPLIFY = "simplify"
  //
  val KEY_PROFILING = "profiling"
  //
  val KEY_PREVMODEL_HEURISTIC = "prevmodel_heur"
  val KEY_UPDATE_SUPPRULE_HEURISTIC = "update_supprule_heur"

  // REASONERS
  val CLINGO = "clingo"
  val INCREMENTAL = "incr"

  // INSTANCES
  val SAMPLE = "sample"

}
