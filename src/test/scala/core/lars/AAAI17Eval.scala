package core.lars

import common.Util.stopTime
import core._
import core.asp._
import core.lars.Util.{asAspRule, rule}
import jtms._
import org.scalatest.FunSuite

import scala.io.Source
import scala.util.Random


/**
  * Created by hb on 9/14/16.
  */
class AAAI17Eval extends FunSuite {

  test("synth") {

    val fromFile = false
    val warmUps = 2
    val loops = 15

    val program = if (fromFile) {
      val filename = "/ground-programs/synth_500_1000_4_01.rules"
      readProgramFromFile(filename)
    } else {
      val nrOfAtoms =7500
      val nrOfRules = 10000
      val maxNrOfBodyAtomsPerRule = 4
      val p = randomProgram(nrOfAtoms,nrOfRules,maxNrOfBodyAtomsPerRule)
      p.rules foreach println
      p
    }

    //

    val n = program.rules.size
    val initRules = program.rules.take(n/2)
    val evalRules = program.rules.drop(n/2)

    var totalTime = 0L
    var totalRetractions = 0

    for (impl <- Seq("doyle","greedy")) {

      print("impl:"+impl)

      for (i <- (1 + (warmUps * -1)) to loops) {

        print(" "+i)

        val tms = impl match {
          case "greedy" => new JtmsGreedy()
          case "doyle" => new JtmsDoyle()
        }
        initRules foreach tms.add
        val time = stopTime{
          evalRules foreach tms.add
          evalRules foreach tms.remove
        }
        if (i >= 1) {
          totalTime = totalTime + time
        }

        if (impl == "doyle"){
          totalRetractions = totalRetractions + tms.asInstanceOf[JtmsDoyle].retractionsAffected
        }

      }

      val avgTime = (1.0*totalTime)/(1.0*loops)/(1000.0)
      println(f"\navg time $impl: $avgTime sec")

      if (impl == "doyle") {
        val avgRetractions = (1.0*totalRetractions)/(1.0*loops)/(1000.0)
        println(f"avg retractions: $avgRetractions")
      }

    }

  }

  def readProgramFromFile(filename: String): NormalProgram = {
    val source = Source.fromURL(getClass.getResource(filename))
    val rules = source.getLines().toSeq map (l => asAspRule(rule(l)))
    AspProgram(rules.toList)
  }

  def printModel(tms: Jtms): Unit = {
    println("model for "+tms.getClass.getSimpleName)
    tms.getModel match {
      case Some(m) => println(m); println("#atoms: "+m.size)
      case None => println("none")
    }
  }

  def randomProgram(nrOfAtoms: Int, nrOfRules: Int, maxNrOfBodyAtomsPerRule: Int): NormalProgram = {
    val rand = new Random()
    var rules = Set[NormalRule]()
    while (rules.size < nrOfRules) {
      rules = rules + randomRule(rand,nrOfAtoms,maxNrOfBodyAtomsPerRule)
    }
    AspProgram(rules.toList)
  }

  def randomRule(rand: Random, nrOfAtoms: Int, maxNrOfBodyAtomsPerRule: Int): NormalRule = {
    var numbers = Seq[Int]()

    val nrBody = rand.nextInt(maxNrOfBodyAtomsPerRule) + 1  //maxNr=4 => (0..3) + 1 => 1..4
    val nrPos = rand.nextInt(nrBody + 1) //nrBody = 1..4 ==> (0..0)..(0..3) + 1 ==> (0..1)..(0..4)

    while (numbers.size < nrBody + 1) {
      val k = rand.nextInt(nrOfAtoms)
      if (!numbers.contains(k)) {
        numbers = numbers :+ k
      }
    }
    val head:Atom = Atom("a"+numbers(0))
    val posInts: Set[Int] = numbers.tail.take(nrPos).toSet
    val negInts: Set[Int] = numbers.drop(nrPos + 1).toSet
    val pos:Set[Atom] = posInts map (i => Atom("a"+i))
    val neg:Set[Atom] = negInts map (i => Atom("a"+i))
    UserDefinedAspRule[Atom](head,pos,neg)
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
