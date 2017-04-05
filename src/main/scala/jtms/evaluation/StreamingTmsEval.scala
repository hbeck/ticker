package jtms.evaluation

import java.io.{File, PrintWriter}
import java.util.concurrent.TimeUnit

import common.Util.stopTime
import core.asp._
import jtms._
import jtms.algorithms.{JtmsDoyle, JtmsGreedy, JtmsLearn}
import jtms.evaluation.instances.{MMediaDeterministicEvalInst, MMediaNonDeterministicEvalInst}
import jtms.networks.{OptimizedNetwork, SimpleNetwork}
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
    var impl = "DoyleOpt"
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
          println("eg: DoyleOpt 2 10 30 180 mmediaDet")
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

    for (i <- (1 + (warmUps * -1)) to iterations) {

      print(" " + i)

      val tms = impl match {
        case "DoyleSimple" => new JtmsDoyle(new SimpleNetwork(), instance.random)
        case "Doyle" => new JtmsDoyle(new OptimizedNetwork(), instance.random)
        case "Greedy" => new JtmsGreedy(new OptimizedNetwork(), instance.random)
        case "Learn" => new JtmsLearn()
      }

      val result: Map[String, Long] = runIteration(instance, tms)

      if (i >= 1) {
        totalTime += result(_evaluationIterationTime)
        totalModels += result(_models)
        totalFailures += result(_failures)
        totalRuleGenTime += result(_ruleGenTime)
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

      if (impl == "Doyle") {
        totalRetractions = totalRetractions + tms.asInstanceOf[JtmsDoyle].retractionsAffected
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

    val avgTimeIteration = totalTime %% iterations sec
    val avgTimeStaticRules = totalTimeStaticRules %% iterations sec
    val avgTimeAllTimePoints = totalTimeAllTimePoints %% (iterations * instance.timePoints) sec
    val avgTimeAddFact = totalTimeAddFact %% totalNrAddFact sec
    val avgTimeAddRule = totalTimeAddRule %% totalNrAddRule sec
    val avgTimeRemoveRule = totalTimeRemoveRule %% totalNrRemoveRule sec
    val avgTimeRemoveFact = totalTimeRemoveFact %% totalNrRemoveFact sec
    val avgTimeGetModel = totalTimeGetModel %% (iterations * instance.timePoints) sec
    val avgRuleGenTime = totalRuleGenTime %% iterations sec
    val totalUpdates = totalModels + totalFailures
    val ratioModels = totalModels %% totalUpdates
    val ratioFailures = totalFailures %% totalUpdates

    println(f"\navg time per iteration: $avgTimeIteration sec")
    println(f"avg time add static rules: $avgTimeStaticRules sec")
    println(f"avg time per time point: $avgTimeAllTimePoints sec")
    println(f"avg time add fact: $avgTimeAddFact sec")
    println(f"avg time add rule: $avgTimeAddRule sec")
    println(f"avg time remove rule: $avgTimeRemoveRule sec")
    println(f"avg time remove fact: $avgTimeRemoveFact sec")
    println(f"avg time get model: $avgTimeGetModel sec")
    println(f"avg rule gen time: $avgRuleGenTime sec")
    println(f"ratio models: $ratioModels")
    println(f"ratio failures: $ratioFailures")

    if (impl == "Doyle") {
      val avgRetractions = (1.0 * totalRetractions) / (1.0 * iterations)
      println(f"avg retractions: $avgRetractions")
    }

  }

  val _evaluationIterationTime = "evaluationIterationTime"
  val _models = "models"
  val _failures = "failures"
  val _ruleGenTime = "ruleGenTime" //only internal info
  val _timeStaticRules = "timeStaticRules"
  val _timeAllTimePoints = "timeAllTimePoints"
  val _timeAddFacts = "timeAddFact"
  val _timeAddRules = "timeAddRule"
  val _timeRemoveRules = "timeRemoveRule"
  val _timeRemoveFacts = "timeRemoveFact"
  val _timeGetModel = "timeGetModel"
  val _nrOfStaticRules = "nrOfInitRules"
  val _nrOfAddedFacts = "nrAddFact"
  val _nrOfAddedRules = "nrAddRule"
  val _nrOfRemovedRules = "nrRemoveRule"
  val _nrOfRemovedFacts = "nrRemoveFact"

  def runIteration(inst: StreamingTmsEvalInstance, tms: JtmsUpdateAlgorithm): Map[String, Long] = {

    var models = 0L
    var failures = 0L

    val timeStaticRules: Long = stopTime {
      inst.staticRules foreach tms.add
    }
    val nrOfStaticRules: Long = inst.staticRules.size

    var timeAllTimePoints = 0L
    var ruleGenTime = 0L
    var timeAddFacts = 0L
    var timeAddRules = 0L
    var timeRemoveRules = 0L
    var timeRemoveFacts = 0L
    var timeGetModel = 0L
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
      var loopTimeGetModel = 0L

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

    Map() + (_evaluationIterationTime -> evaluationIterationTime) + (_models -> models) + (_failures -> failures) + (_ruleGenTime -> ruleGenTime) +
      (_timeStaticRules -> timeStaticRules) + (_timeAllTimePoints -> timeAllTimePoints) +
      (_timeAddFacts -> timeAddFacts) + (_timeAddRules -> timeAddRules) +
      (_timeRemoveRules -> timeRemoveRules) + (_timeRemoveFacts -> timeRemoveFacts) + (_timeGetModel -> timeGetModel) +
      (_nrOfStaticRules -> nrOfStaticRules) + (_nrOfAddedFacts -> nrOfAddedFacts) + (_nrOfAddedRules -> nrOfAddedRules) +
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