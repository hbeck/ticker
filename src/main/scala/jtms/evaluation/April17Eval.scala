package jtms.evaluation

import java.io.{File, PrintWriter}
import java.util.concurrent.TimeUnit

import common.Util.stopTime
import core.asp._
import jtms._
import jtms.algorithms.{JtmsDoyle, JtmsGreedy, JtmsLearn}
import jtms.networks.OptimizedNetwork
import runner.Load

import scala.io.Source


/**
  * Created by hb on 4/3/17.
  */
object April17Eval {

  val loader = Load(TimeUnit.SECONDS)

  def main(args: Array[String]): Unit = {
    evaluate(args)
  }

  def evaluate(args: Array[String]): Unit = {
    var impl = "greedy"
    var warmUps = 2
    var iterations = 10
    var instanceNames = Seq("p1")
    var dir = "src/test/resources/ground-programs/"
    if (args.nonEmpty) {
      try {
        impl = args(0)
        warmUps = Integer.parseInt(args(1))
        iterations = Integer.parseInt(args(2))
        dir = args(3)
        instanceNames = args.drop(4).toSeq
      } catch {
        case e: Exception => {
          println("args: impl warmUps iterations dir instanceName1 instanceName2 ...")
          println("eg: greedy 2 10 ~/programs p1 p2 ...")
          System.exit(1)
        }
      }
    }
    println(f"impl: $impl, warmUps: $warmUps, iterations: $iterations, dir: $dir, instanceNames: $instanceNames")
    run(impl, warmUps, iterations, dir, instanceNames)
  }

  def run(impl: String, warmUps: Int, iterations: Int, dir: String, instanceNames: Seq[String]) {

    for (instanceName <- instanceNames) {
      //val filename = f"/ground-programs/${instanceName}.rules"
      print(f"\ninstance: " + instanceName + " ")
      val filename = dir + "/" + instanceName + ".lp"
      val program = readProgramFromFile(filename)
      runImplementation(impl, warmUps, iterations, program)
    }

  }

  def runImplementation(impl: String, warmUps: Int, iterations: Int, program: NormalProgram): Unit = {

    var totalTime = 0L
    var totalRetractions = 0L
    var totalModels = 0L
    var totalFailures = 0L

    for (i <- (1 + (warmUps * -1)) to iterations) {

      print(" " + i)

      val tms = impl match {
        case "doyle" => new JtmsDoyle(new OptimizedNetwork())
        case "greedy" => new JtmsGreedy(new OptimizedNetwork())
        case "learn" => new JtmsLearn()
      }

      val result: Map[String, Long] = runIteration(program, tms)

      if (i >= 1) {
        totalTime += result(_time)
        totalModels += result(_models)
        totalFailures += result(_failures)
      }

      if (impl == "doyle") {
        totalRetractions = totalRetractions + tms.asInstanceOf[JtmsDoyle].retractionsAffected
      }

    }

    val avgTime = (1.0 * totalTime) / (1.0 * iterations) / (1000.0)
    val totalUpdates = totalModels + totalFailures
    val ratioModels = (1.0 * totalModels) / (1.0 * totalUpdates)
    val ratioFailures = (1.0 * totalFailures) / (1.0 * totalUpdates)

    println(f"\navg time: $avgTime sec")
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

  def runIteration(program: NormalProgram, tms: JtmsUpdateAlgorithm): Map[String, Long] = {

    var models = 0L
    var failures = 0L

    val time = stopTime {

      program.rules foreach { r =>
        tms.add(r)
        if (tms.getModel.isDefined) models += 1
        else failures += 1
      }

      program.rules foreach { r =>
        tms.remove(r)
        if (tms.getModel.isDefined) models += 1
        else failures += 1
      }

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

    Map() + (_time -> time) + (_models -> models) + (_failures -> failures)
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
