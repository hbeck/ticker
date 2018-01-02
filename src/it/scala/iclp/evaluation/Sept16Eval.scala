package iclp.evaluation

import java.io.{File, PrintWriter}
import java.util.concurrent.TimeUnit

import common.Util.stopTime
import core._
import core.asp._
import reasoner.incremental.jtms._
import reasoner.incremental.jtms.algorithms.{Jtms, JtmsDoyle, JtmsGreedy, JtmsLearn}
import reasoner.incremental.jtms.networks.OptimizedNetwork
import runner.Load

import scala.io.Source
import scala.util.Random


/**
  * Created by hb on 9/14/16.
  */
object Sept16Eval {

  val loader = Load(TimeUnit.SECONDS)

  def main(args: Array[String]): Unit = {
    //generateProgram("out")
    evaluate(args)
  }

  def generateProgram(filename: String): Unit = {
    val nrOfAtoms = 5000
    val nrOfRules = 10000
    val maxNrOfBodyAtomsPerRule = 4
    val maxNrOfPosBodyAtomsPerRule = 3
    //val negationProbability = 0.0
    val choicePairs = 150

    val p = randomProgram4(nrOfAtoms, nrOfRules, maxNrOfPosBodyAtomsPerRule, choicePairs)
    //val p = randomProgram2(nrOfAtoms,nrOfRules,maxNrOfBodyAtomsPerRule)
    writeProgramToFile(p, filename + ".rules")

    var ranThrough = false
    var attempt = 0
    while (attempt < 100 && !ranThrough) {
      attempt += 1
      try {
        val tms = new JtmsDoyle(new OptimizedNetwork())
        tms.doConsistencyCheck = true
        tms.doJtmsSemanticsCheck = true
        tms.doSelfSupportCheck = true

        runIteration(p, tms)

        println(f"#${attempt} retractions: ${tms.retractionsAffected}")
        if (!tms.failed) {
          ranThrough = true
        } else {
          println(f"#${attempt} failed")
        }
      } catch {
        case e: StackOverflowError => println(f"#${attempt}: stack overflow")
      }
    }
    println(f"found instance: $ranThrough")
  }

  def evaluate(args: Array[String]): Unit = {
    var impl = "greedy"
    var warmUps = 2
    var iterations = 10
    var instanceNames = Seq("a9000")
    var dir = "~"
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
          println("eg: greedy 2 10 ~/programs a9000 (=default)")
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
      val filename = dir + "/" + instanceName + ".rules"
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
    val updates = totalModels + totalFailures
    val ratioModels = (1.0 * totalModels) / (1.0 * updates)
    val ratioFailures = (1.0 * totalFailures) / (1.0 * updates)

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

  def runIteration(program: NormalProgram, tms: Jtms): Map[String, Long] = {

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

  def printModel(tms: Jtms): Unit = {
    println("model for " + tms.getClass.getSimpleName)
    tms.getModel match {
      case Some(m) => println(m); println("#atoms: " + m.size)
      case None => println("none")
    }
  }

  def randomProgram4(nrOfAtoms: Int, nrOfRules: Int, maxNrOfPosBodyAtomsPerRule: Int, choicePairs: Int): NormalProgram = {
    val rand = new Random()
    var rules = Set[NormalRule]()
    def mkNewAtoms = rand.shuffle((1 to nrOfAtoms) map (i => Atom("a" + i)) toList)
    var availableAtoms = mkNewAtoms

    while (rules.size < nrOfRules - (2 * choicePairs)) {

      val nrPos = rand.nextInt(maxNrOfPosBodyAtomsPerRule) + 1 //maxNr=4 => (0..3) + 1 => 1..4

      if (availableAtoms.size < (nrPos + 1)) availableAtoms = mkNewAtoms

      val head = availableAtoms.head
      val pos = availableAtoms.tail.take(nrPos).toSet
      availableAtoms = availableAtoms.tail.drop(nrPos)

      rules = rules + UserDefinedAspRule[Atom](head, pos, Set())

    }

    for (k <- 1 to choicePairs) {
      if (availableAtoms.size < 2) availableAtoms = mkNewAtoms
      val one = availableAtoms.head
      availableAtoms = availableAtoms.tail
      val two = availableAtoms.head
      availableAtoms = availableAtoms.tail

      rules = rules + UserDefinedAspRule(one, Set(), Set(two)) + UserDefinedAspRule(two, Set(), Set(one))
    }

    AspProgram(rules.toList)
  }


  def randomProgram3(nrOfAtoms: Int, nrOfRules: Int, maxNrOfPosBodyAtomsPerRule: Int, negationProbability: Double): NormalProgram = {
    val rand = new Random()
    var rules = Set[NormalRule]()
    def mkNewAtoms = rand.shuffle((1 to nrOfAtoms) map (i => Atom("a" + i)) toList)
    var availableAtoms = mkNewAtoms
    while (rules.size < nrOfRules) {

      val useNegation = rand.nextDouble() < negationProbability

      val base = if (useNegation) 0 else 1

      val nrPos = rand.nextInt(maxNrOfPosBodyAtomsPerRule) + base
      val nrNeg = if (useNegation) 1 else 0

      if (availableAtoms.size < (nrPos + nrNeg + 1)) {
        availableAtoms = mkNewAtoms
      }

      val head = availableAtoms.head
      val pos = availableAtoms.tail.take(nrPos).toSet
      availableAtoms = availableAtoms.tail.drop(nrPos)
      val neg = availableAtoms.take(nrNeg).toSet
      availableAtoms = availableAtoms.drop(nrNeg)

      rules = rules + UserDefinedAspRule[Atom](head, pos, neg)

    }
    AspProgram(rules.toList)
  }

  //use all atoms first before starting using the same again
  def randomProgram2(nrOfAtoms: Int, nrOfRules: Int, maxNrOfBodyAtomsPerRule: Int): NormalProgram = {
    val rand = new Random()
    var rules = Set[NormalRule]()
    def mkNewAtoms = rand.shuffle((1 to nrOfAtoms) map (i => Atom("a" + i)) toList)
    var availableAtoms = mkNewAtoms
    while (rules.size < nrOfRules) {

      val nrBody = rand.nextInt(maxNrOfBodyAtomsPerRule) + 1 //maxNr=4 => (0..3) + 1 => 1..4

      if (availableAtoms.size < (nrBody + 1)) {
        availableAtoms = mkNewAtoms
      }

      val nrPos = rand.nextInt(nrBody + 1) //nrBody = 1..4 ==> (0..0)..(0..3) + 1 ==> (0..1)..(0..4)
      val nrNeg = nrBody - nrPos

      val head = availableAtoms.head
      val pos = availableAtoms.tail.take(nrPos).toSet
      availableAtoms = availableAtoms.tail.drop(nrPos)
      val neg = availableAtoms.take(nrNeg).toSet
      availableAtoms = availableAtoms.drop(nrNeg)

      rules = rules + UserDefinedAspRule[Atom](head, pos, neg)

    }
    AspProgram(rules.toList)
  }

  def randomProgram(nrOfAtoms: Int, nrOfRules: Int, maxNrOfBodyAtomsPerRule: Int): NormalProgram = {
    val rand = new Random()
    var rules = Set[NormalRule]()
    while (rules.size < nrOfRules) {
      rules = rules + randomRule(rand, nrOfAtoms, maxNrOfBodyAtomsPerRule)
    }
    AspProgram(rules.toList)
  }

  def randomRule(rand: Random, nrOfAtoms: Int, maxNrOfBodyAtomsPerRule: Int): NormalRule = {
    var numbers = Seq[Int]()

    val nrBody = rand.nextInt(maxNrOfBodyAtomsPerRule) + 1 //maxNr=4 => (0..3) + 1 => 1..4
    val nrPos = rand.nextInt(nrBody + 1) //nrBody = 1..4 ==> (0..0)..(0..3) + 1 ==> (0..1)..(0..4)

    while (numbers.size < nrBody + 1) {
      val k = rand.nextInt(nrOfAtoms)
      if (!numbers.contains(k)) {
        numbers = numbers :+ k
      }
    }
    val head: Atom = Atom("a" + numbers(0))
    val posInts: Set[Int] = numbers.tail.take(nrPos).toSet
    val negInts: Set[Int] = numbers.drop(nrPos + 1).toSet
    val pos: Set[Atom] = posInts map (i => Atom("a" + i))
    val neg: Set[Atom] = negInts map (i => Atom("a" + i))
    UserDefinedAspRule[Atom](head, pos, neg)
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
