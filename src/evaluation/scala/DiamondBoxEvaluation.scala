import core.Atom
import core.lars._
import evaluation.{AlgorithmResult, DumpData, PrepareEvaluator}

import scala.collection.immutable.HashMap
import scala.util.Random

/**
  * Created by fm on 24/02/2017.
  */
object DiamondBoxEvaluation extends DiamondBoxSpec {

  val all_02 = HashMap(x -> 0.2, y -> 0.2, z -> 0.2)

  def main(args: Array[String]): Unit = {
    timings(args)

  }

  def timings(args: Array[String]): Unit = {
    // evaluate everything one time as pre-pre-warmup
    evaluateTimings(Seq("Tms", "Doyle") toArray)

    val dump = DumpData("Configuration", "Programs")
    val dumpToCsv = dump.printResults("diamond-box-output.csv") _

    if (args.length == 0) {
      val allOptions = Seq(
        Seq("Tms", "Doyle"),
        Seq("Clingo", "Push")
      )

      val allResults = allOptions map (o => evaluateTimings(o.toArray))

      dump.plot(allResults)

      dumpToCsv(allResults)

    } else {
      val results = evaluateTimings(args)
      dump.plot(Seq(results))
      dumpToCsv(Seq(results))
    }
  }

  def evaluateTimings(args: Array[String], timePoints: Long = 500) = {

    val random = new Random(1)

    val evaluationOptions = HashMap(
      ("k/n=100, i/j=2: P: 0.2", all_02) -> buildProgram(100, 100, 2, 2),
      ("k/n=1000, i/j=2: P: 0.2", all_02) -> buildProgram(1000, 1000, 2, 2),
      ("k/n=100, i/j=5: P: 0.2", all_02) -> buildProgram(100, 100, 5, 5),
      ("k/n=1000, i/j=5: P: 0.2", all_02) -> buildProgram(1000, 1000, 5, 5)
    )

    val evaluationCombination = evaluationOptions map {
      case (instance, program) => {
        val signals = PrepareEvaluator.generateSignals(instance._2, random, 0, timePoints)

        (PrepareEvaluator.fromArguments(args, instance._1, program), signals)
      }
    }

    val option = args.mkString(" ")

    Console.out.println("Algorithm: " + option)

    val results = evaluationCombination map {
      case (evaluator, signals) => evaluator.streamAsFastAsPossible(1, 2)(signals)
    }

    AlgorithmResult(option, results toList)
  }
}

trait DiamondBoxSpec {
  val x = Atom("x")
  val y = Atom("y")
  val z = Atom("z")
  val some = Atom("some")
  val xdom = Atom("xdom")
  val ydom = Atom("ydom")
  val both = Atom("both")
  val dz = Atom("dz")
  val bz = Atom("bz")
  val imperfz = Atom("imperfz")

  def buildProgram(k: Long, n: Long, i: Long, j: Long): LarsProgram = {

    def slidingTime(windowSize: Long, temp: TemporalModality, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), temp, atom)

    def slidingTuple(windowSize: Long, temp: TemporalModality, atom: Atom) = WindowAtom(SlidingTupleWindow(windowSize), temp, atom)


    LarsProgram.from(
      some <= slidingTime(k, Diamond, x) and slidingTuple(n, Diamond, y),
      xdom <= slidingTime(i, Box, x) not slidingTuple(j, Diamond, y),
      ydom <= slidingTuple(j, Diamond, y) not slidingTime(i, Box, x),
      both <= slidingTime(i, Box, x) and slidingTime(i, Box, y),
      dz <= some and slidingTime(i, Diamond, z),
      bz <= some and slidingTime(i, Box, z),
      imperfz <= dz not bz
    )
  }

}
