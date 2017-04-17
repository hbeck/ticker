package jtms.evaluation

import java.io.{File, PrintWriter}
import java.util.concurrent.TimeUnit

import common.Util.stopTime
import core.Atom
import core.asp._
import jtms._
import jtms.algorithms._
import runner.Load

import scala.io.Source


/**
  * Created by hb on 4/4/17.
  */
object StreamingTmsEval {

  val loader = Load(TimeUnit.SECONDS)

  def main(args: Array[String]): Unit = {
    evaluate(args)
  }

  def evaluate(args: Array[String]): Unit = {
    val argMap = Config.buildArgMap(args)
    run(Config(argMap))
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

    var instance: Option[StreamingTmsEvalInst] = None

    for (i <- (1 + (cfg.preRuns * -1)) to cfg.runs) {

      if (cfg.withDebug) { print(" " + i) }

      instance = Some(cfg.makeInstance(i)) //init only here for having different random seeds

      val result: Map[String, Long] = runIteration(instance.get,cfg)

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
    val avgTimeRuleGenPerTimePoint = totalTimeRuleGen %% (runs * tp) sec
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

    println(f"\nnr of static rules: ${instance.get.staticRules.size}")
    println(f"iteration avg:")
    println(f"total time: $avgTimeIteration sec")
    println(f"rule generation (not included): $avgTimeRuleGen sec")
    println(f"add static rules: $avgTimeStaticRules sec")
    println(f"avg per time point: $avgTimeAllTimePoints sec")
    println(f"avg rule gen per time point (not incl): $avgTimeRuleGenPerTimePoint sec")
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

  def fact(head: Atom): NormalRule = UserDefinedAspFact[Atom](head)

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

      var factsToAdd = Seq[NormalRule]()
      var rulesToAdd = Seq[NormalRule]()
      var rulesToRemove = Seq[NormalRule]()
      var factsToRemove = Seq[NormalRule]()

      timeRuleGen = timeRuleGen + stopTime {
        factsToAdd = inst.manualTmsFactsToAddAt(t)
        rulesToAdd = inst.manualTmsRulesToAddAt(t)
        rulesToRemove = inst.manualTmsRulesToRemoveAt(t)
        factsToRemove = inst.manualTmsFactsToRemoveAt(t)
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

      if (cfg.verifyModel) {
        inst.verifyModel(tms.getModel, t)
      }

    }

    val evaluationIterationTime = timeStaticRules + timeAllTimePoints

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
