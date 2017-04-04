package jtms.evaluation

import java.io.{File, PrintWriter}
import java.util.concurrent.TimeUnit

import common.Util.stopTime
import core.asp._
import jtms._
import jtms.algorithms.{JtmsDoyle, JtmsGreedy, JtmsLearn}
import jtms.evaluation.instances.{MMediaDeterministicEvalInst, MMediaNonDeterministicEvalInst}
import jtms.networks.OptimizedNetwork
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
    var impl = "doyle"
    var warmUps = 0
    var iterations = 1
    var windowSize = 10
    var timePoints = 3600
    var instanceNames = Seq("mmediaNonDet")
    //var dir = "src/test/resources/ground-programs/"
    if (args.nonEmpty) {
      try {
        impl = args(0)
        warmUps = Integer.parseInt(args(1))
        iterations = Integer.parseInt(args(2))
        windowSize = Integer.parseInt(args(3))
        timePoints = Integer.parseInt(args(4))
        instanceNames = args.drop(5).toSeq
      } catch {
        case e: Exception => {
          println("args: impl warmUps iterations windowSize timePoints inst1 inst2 ...")
          println("eg: doyle 2 10 30 180 mmediaDet")
          System.exit(1)
        }
      }
    }
    println(f"impl: $impl, warmUps: $warmUps, iterations: $iterations, windowSize: $windowSize, timePoints: $timePoints")
    run(impl, warmUps, iterations, windowSize, timePoints, instanceNames)
  }

  def run(impl: String, warmUps: Int, iterations: Int, windowSize: Int, timePoints: Int, instanceNames: Seq[String]) {
    for (instanceName <- instanceNames) {
      println(instanceName)
      val inst = makeInstance(instanceName,windowSize,timePoints)
      runImplementation(impl, warmUps, iterations, inst)
    }
  }

  def makeInstance(instanceName: String, windowSize: Int, timePoints: Int): StreamingTmsEvalInstance = {
    instanceName match {
      case "mmediaDet" => MMediaDeterministicEvalInst(windowSize, timePoints)
      case "mmediaNonDet" => MMediaNonDeterministicEvalInst(windowSize, timePoints)
      case _ => println(f"unknown instance name $instanceName"); throw new RuntimeException
    }
  }

  def runImplementation(impl: String, warmUps: Int, iterations: Int, instance: StreamingTmsEvalInstance): Unit = {

    var totalTime = 0L
    var totalRetractions = 0L
    var totalModels = 0L
    var totalFailures = 0L
    var totalRuleGenTime = 0L

    var totalTimeAddFact = 0L
    var totalTimeAddRule = 0L
    var totalTimeRemoveRule = 0L
    var totalTimeRemoveFact = 0L
    var totalNrAddFact = 0L
    var totalNrAddRule = 0L
    var totalNrRemoveRule = 0L
    var totalNrRemoveFact = 0L

    for (i <- (1 + (warmUps * -1)) to iterations) {

      print(" " + i)

      val tms = impl match {
        case "doyle" => new JtmsDoyle(new OptimizedNetwork(), instance.random)
        case "greedy" => new JtmsGreedy(new OptimizedNetwork(), instance.random)
        case "learn" => new JtmsLearn()
      }

      val result: Map[String, Long] = runIteration(instance, tms)

      if (i >= 1) {
        totalTime += result(_time)
        totalModels += result(_models)
        totalFailures += result(_failures)
        totalRuleGenTime += result(_ruleGenTime)
        totalTimeAddFact += result(_timeAddFacts)
        totalTimeAddRule += result(_timeAddRules)
        totalTimeRemoveRule += result(_timeRemoveRules)
        totalTimeRemoveFact += result(_timeRemoveFacts)
        totalNrAddFact += result(_nrOfAddedFacts)
        totalNrAddRule += result(_nrOfAddedRules)
        totalNrRemoveRule += result(_nrOfRemovedRules)
        totalNrRemoveFact += result(_nrOfRemovedFacts)
      }

      if (impl == "doyle") {
        totalRetractions = totalRetractions + tms.asInstanceOf[JtmsDoyle].retractionsAffected
      }

    }

    val avgTime = (1.0 * totalTime) / (1.0 * iterations) / (1000.0)
    val avgTimeAddFact = (1.0 * totalTimeAddFact) / (1.0 * totalNrAddFact) / (1000.0)
    val avgTimeAddRule = (1.0 * totalTimeAddRule) / (1.0 * totalNrAddRule) / (1000.0)
    val avgTimeRemoveRule = (1.0 * totalTimeRemoveRule) / (1.0 * totalNrRemoveRule) / (1000.0)
    val avgTimeRemoveFact = (1.0 * totalTimeRemoveFact) / (1.0 * totalNrRemoveFact) / (1000.0)
    val avgRuleGenTime = (1.0 * totalRuleGenTime) / (1.0 * iterations) / (1000.0)
    val totalUpdates = totalModels + totalFailures
    val ratioModels = (1.0 * totalModels) / (1.0 * totalUpdates)
    val ratioFailures = (1.0 * totalFailures) / (1.0 * totalUpdates)

    println(f"\navg time: $avgTime sec")
    println(f"avg time add fact: $avgTimeAddFact sec")
    println(f"avg time add rule: $avgTimeAddRule sec")
    println(f"avg time remove rule: $avgTimeRemoveRule sec")
    println(f"avg time remove fact: $avgTimeRemoveFact sec")
    println(f"avg rule gen time: $avgRuleGenTime sec")
    println(f"ratio models: $ratioModels")
    println(f"ratio failures: $ratioFailures")

    if (impl == "doyle") {
      val avgRetractions = (1.0 * totalRetractions) / (1.0 * iterations)
      println(f"avg retractions: $avgRetractions")
    }
  }

  val _time = "time"
  val _models = "models"
  val _failures = "failures"
  val _ruleGenTime = "ruleGenTime" //only internal info
  val _timeAddFacts = "timeAddFact"
  val _timeAddRules = "timeAddRule"
  val _timeRemoveRules = "timeRemoveRule"
  val _timeRemoveFacts = "timeRemoveFact"
  val _nrOfAddedFacts = "nrAddFact"
  val _nrOfAddedRules = "nrAddRule"
  val _nrOfRemovedRules = "nrRemoveRule"
  val _nrOfRemovedFacts = "nrRemoveFact"

  def runIteration(inst: StreamingTmsEvalInstance, tms: JtmsUpdateAlgorithm): Map[String, Long] = {

    var models = 0L
    var failures = 0L

    //init
    inst.staticRules foreach tms.add

    var iterationTime = 0L
    var ruleGenTime = 0L
    var timeAddFacts = 0L
    var timeAddRules = 0L
    var timeRemoveRules = 0L
    var timeRemoveFacts = 0L
    var nrOfAddedFacts = 0L
    var nrOfAddedRules = 0L
    var nrOfRemovedRules = 0L
    var nrOfRemovedFacts = 0L

    for (t <- 0 to inst.timePoints) {

      var factsToAdd = Seq[NormalRule]()
      var rulesToAdd = Seq[NormalRule]()
      var rulesToRemove = Seq[NormalRule]()
      var factsToRemove = Seq[NormalRule]()

      ruleGenTime = ruleGenTime + stopTime {
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

      factsToAdd foreach { r =>
        //println("add "+r)
        loopTimeAddFacts += stopTime { tms.add(r) }
        if (tms.getModel.isDefined) models += 1
        else failures += 1
      }

      rulesToAdd foreach { r =>
        //println("add "+r)
        loopTimeAddRules += stopTime { tms.add(r) }
        if (tms.getModel.isDefined) models += 1
        else failures += 1
      }

      rulesToRemove foreach { r =>
        //println("remove "+r)
        loopTimeRemoveRules += stopTime { tms.remove(r) }
        if (tms.getModel.isDefined) models += 1
        else failures += 1
      }

      factsToRemove foreach { r =>
        //println("remove "+r)
        loopTimeRemoveFacts += stopTime { tms.remove(r) }
        if (tms.getModel.isDefined) models += 1
        else failures += 1
      }

      val loopTime = loopTimeAddFacts + loopTimeAddRules + loopTimeRemoveRules + loopTimeRemoveFacts

      iterationTime += loopTime
      timeAddFacts += loopTimeAddFacts
      timeAddRules += loopTimeAddRules
      timeRemoveRules += loopTimeRemoveRules
      timeRemoveFacts += loopTimeRemoveFacts

      inst.verifyModel(tms,t)

    }

    if (tms.isInstanceOf[JtmsDoyle]) {
      val jtms = tms.asInstanceOf[JtmsDoyle]
      jtms.doConsistencyCheck = true
      jtms.doJtmsSemanticsCheck = true
      jtms.doSelfSupportCheck = true
      jtms.checkConsistency()
      jtms.checkJtmsSemantics()
      jtms.checkSelfSupport()
    }

    Map() + (_time -> iterationTime) + (_models -> models) + (_failures -> failures) + (_ruleGenTime -> ruleGenTime) +
      (_timeAddFacts -> timeAddFacts) + (_timeAddRules -> timeAddRules) + (_timeRemoveRules -> timeRemoveRules) +
      (_timeRemoveFacts -> timeRemoveFacts) + (_nrOfAddedFacts -> nrOfAddedFacts) + (_nrOfAddedRules -> nrOfAddedRules) +
      (_nrOfRemovedRules -> nrOfRemovedRules) + (_nrOfRemovedFacts -> nrOfRemovedFacts)

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

