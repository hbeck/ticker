package jtms.evaluation

import java.io.{File, PrintWriter}
import java.util.concurrent.TimeUnit

import common.Util.stopTime
import core.{Atom, Predicate}
import core.asp._
import jtms._
import jtms.algorithms.{JtmsDoyle, JtmsGreedy, JtmsLearn}
import jtms.networks.OptimizedNetwork
import runner.Load

import scala.io.Source


/**
  * Created by hb on 4/4/17.
  */
object MMediaEval {

  val loader = Load(TimeUnit.SECONDS)

  def main(args: Array[String]): Unit = {
    evaluate(args)
  }

  def evaluate(args: Array[String]): Unit = {
    var impl = "doyle"
    var warmUps = 2
    var iterations = 10
    var windowSize = 30
    var timePoints = 1
    //var instanceNames = Seq("p1")
    //var dir = "src/test/resources/ground-programs/"
    if (args.nonEmpty) {
      try {
        impl = args(0)
        warmUps = Integer.parseInt(args(1))
        iterations = Integer.parseInt(args(2))
        windowSize = Integer.parseInt(args(3))
        timePoints = Integer.parseInt(args(4))
        //instanceNames = args.drop(5).toSeq
      } catch {
        case e: Exception => {
          println("args: impl warmUps iterations windowSize timePoints")
          println("eg: doyle 2 10 30 10000")
          System.exit(1)
        }
      }
    }
    println(f"impl: $impl, warmUps: $warmUps, iterations: $iterations, windowSize: $windowSize, timePoints: $timePoints")
    run(impl, warmUps, iterations, windowSize, timePoints)
  }

  def run(impl: String, warmUps: Int, iterations: Int, windowSize: Int, timePoints: Int) {
    val inst = MMediaInstance(windowSize, timePoints)
    println("mmedia")
    runImplementation(impl, warmUps, iterations, inst)
  }

  def runImplementation(impl: String, warmUps: Int, iterations: Int, instance: MMediaInstance): Unit = {

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

      val result: Map[String, Long] = runIteration(instance, tms)

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

  def runIteration(inst: MMediaInstance, tms: JtmsUpdateAlgorithm): Map[String, Long] = {

    var models = 0L
    var failures = 0L

    //init
    inst.staticRules foreach tms.add

    var iterationTime = 0L

    for (t <- 0 to inst.timePoints) {

      //TODO data stream

      val rulesToAdd = inst.rulesToAddAt(t)
      val rulesToRemove = inst.rulesToRemoveAt(t)

      val loopTime = stopTime {

        rulesToAdd foreach { r =>
          //println("add "+r)
          tms.add(r)
          if (tms.getModel.isDefined) models += 1
          else failures += 1
        }

        rulesToRemove foreach { r =>
          //println("remove "+r)
          tms.remove(r)
          if (tms.getModel.isDefined) models += 1
          else failures += 1
        }

      } // end stopTime

      iterationTime += loopTime

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

    Map() + (_time -> iterationTime) + (_models -> models) + (_failures -> failures)
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

case class MMediaInstance(windowSize: Int, timePoints: Int) {

  val done = Atom("done")
  val lfu = Atom("lfu")
  val lru = Atom("lru")
  val fifo = Atom("fifo")
  val random = Atom("random")

  val alpha_at = Predicate("alpha_at")
  val high_at = Predicate("high_at")
  val mid_at = Predicate("mid_at")
  val low_at = Predicate("low_at")
  val rtm_at = Predicate("rtm_at")
  val lt = Predicate("lt")
  val leq = Predicate("leq")

  val spoil_high = Atom("spoil_high")
  val spoil_mid = Atom("spoil_mid")
  val spoil_low = Atom("spoil_low")

  val wrtm = Atom("wrtm")

  /*
done :- lfu.
done :- lru.
done :- fifo.
random :- not done.
   */
  val E = Set[Atom]()

  val staticRules: Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    rules = rules :+ rule(done,lfu) :+ rule(done,lru) :+ rule(done,fifo) :+ rule(random,E,Set(done))
    for (i <- 0 to 30) {
      for (j <- 0 to 30) {
        if (i < j) {
          rules = rules :+ fact(f"lt($i,$j)")
        }
        if (i <= j) {
          rules = rules :+ fact(f"leq($i,$j)")
        }
      }
    }
    rules
  }


//  val rulesToAdd: Map[Int,Seq[NormalRule]] = {
//    (0 to timePoints) map (t => (t,rulesToAddAt(t))) toMap
//  }
//  val rulesToRemove: Map[Int,Seq[NormalRule]] = {
//    (0 to timePoints) map (t => (t,rulesToRemoveAt(t))) toMap
//  }

  def rulesToAddAt(t: Int) = immediatelyExpiringRulesFor(t) ++ rulesExpiringAfterWindow(t)
  def rulesToRemoveAt(t: Int) = immediatelyExpiringRulesFor(t-1) ++ rulesExpiringAfterWindow(t - windowSize - 1)

  def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    rules = rules :+
      rule(lfu,Set[Atom](high_at(t)),Set[Atom](spoil_high)) :+ //lfu :- high_at(N), not spoil_high.
      rule(lru,mid_at(t),spoil_mid) :+ //lru :- mid_at(N), not spoil_mid.
      rule(fifo, Set[Atom](low_at(t),wrtm), Set[Atom](spoil_low)) //fifo :- low_at(N), not spoil_low, wrtm.

    rules
  }

  def rulesExpiringAfterWindow(t: Int): Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    for (v <- 0 to 30) {
      rules = rules :+ rule(high_at(t),Set[Atom](alpha_at(v,t),leq(18,v)),E) //high_at(T) :- alpha_at(V,T), leq(18,V).
      rules = rules :+ rule(mid_at(t),Set[Atom](alpha_at(v,t),leq(12,v), lt(v,18)),E) //mid_at(T) :- alpha_at(V,T), leq(12,V), lt(V,18).
      rules = rules :+ rule(low_at(t),Set[Atom](alpha_at(v,t),lt(v,12)),E) //low_at(T) :- alpha_at(V,T), lt(V,12).

    }
    rules = rules :+
      rule(spoil_high,mid_at(t-1)) :+ //spoil_high :- mid_at(N-1).
      rule(spoil_high,low_at(t-1)) :+ //spoil_high :- low_at(N-1).
      rule(spoil_mid,high_at(t-1)) :+ //spoil_mid :- high_at(N-1).
      rule(spoil_mid,low_at(t-1)) :+ //spoil_mid :- low_at(N-1).
      rule(spoil_low,high_at(t-1)) :+ //spoil_low :- high_at(N-1).
      rule(spoil_low,mid_at(t-1)) :+ //spoil_low :- mid_at(N-1).
      rule(wrtm,rtm_at(t)) //wrtm :- rtm_at(N)

    rules
  }

  def rule(head: Atom, posBody: Set[Atom], negBody: Set[Atom]): NormalRule = {
    UserDefinedAspRule[Atom](head, posBody, negBody)
  }

  def rule(head: Atom, posBody: Atom): NormalRule = {
    UserDefinedAspRule[Atom](head, Set(posBody), Set())
  }

  def rule(head: Atom, posBody: Atom, negBody: Atom): NormalRule = {
    UserDefinedAspRule[Atom](head, Set(posBody), Set(negBody))
  }

  def fact(head: String): NormalRule = UserDefinedAspFact[Atom](Atom(head))

}