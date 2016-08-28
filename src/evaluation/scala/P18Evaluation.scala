import core.lars._
import core.{Atom, StringValue}
import engine.StreamEntry
import evaluation._

import scala.collection.immutable.HashMap
import scala.util.Random

/**
  * Created by FM on 21.08.16.
  */
object P18Evaluation extends P18Program { //TODO note hb: use case(...) instead of ._1, ._2 ...

  val all_001 = HashMap(x_1 -> 0.01, x_2 -> 0.01, x_3 -> 0.01, x_4 -> 0.01, y_1 -> 0.01, y_2 -> 0.01)
  val all_01 = HashMap(x_1 -> 0.1, x_2 -> 0.1, x_3 -> 0.1, x_4 -> 0.1, y_1 -> 0.1, y_2 -> 0.1)
  val all_025 = HashMap(x_1 -> 0.25, x_2 -> 0.25, x_3 -> 0.25, x_4 -> 0.25, y_1 -> 0.25, y_2 -> 0.25)


  def main(args: Array[String]): Unit = {
    failures(args)

  }

  def timings(args: Array[String]): Unit = {
    // evaluate everything one time as pre-pre-warmup
    evaluateTimings(Seq("tms", "greedy") toArray)

    val dump = DumpData("Configuration", "Programs")
    val dumpToCsv = dump.printResults("p18-output.csv") _

    if (args.length == 0) {
      val allOptions = Seq(
        Seq("tms", "greedy"),
        Seq("tms", "doyle"),
        Seq("tms", "learn")
        //        Seq("clingo", "push")
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
      ("P1,P2: 0.01", all_001) -> Seq(P_1, P_2),
      ("P1,P2: 0.25", all_025) -> Seq(P_1, P_2)
    )

    val evaluationCombination = evaluationOptions map { o =>
      val program = LarsProgram(o._2 flatMap (_.toSeq)) //TODO
      val signals = generateSignals(o._1._2, random, 0, timePoints)

      (o._1._1, program, signals) //TODO
    }

    val option = args.mkString(" ")

    Console.out.println("Algorithm: " + option)

    //TODO
    AlgorithmResult(option, evaluationCombination map (c => executeTimings(args ++ Seq("p18"), c._1, c._2, c._3)) toList)
  }

  def generateSignals(probabilities: HashMap[Atom, Double], random: Random, t0: TimePoint, t1: TimePoint) = {
    val signals = (t0.value to t1.value) map (t => {
      val atoms = (probabilities filter (random.nextDouble() <= _._2) keys) toSet //TODO

      StreamEntry(TimePoint(t), atoms)
    })

    signals
  }

  def executeTimings(args: Array[String], instance: String, program: LarsProgram, signals: Seq[StreamEntry]) = {

    Console.out.println(f"Evaluating ${instance}")

    val provider = () => Evaluator.buildEngineFromArguments(args, s => program) //TODO use def

    val e = Evaluator(provider, 1, 2)

    val (append, evaluate) = e.streamInputsAsFastAsPossible(signals)

    TimingsConfigurationResult(instance, append, evaluate)
  }

  def failures(args: Array[String]): Unit = {
    val dump = DumpData("Configuration", "Instances")
    val dumpToCsv = dump.printResults("p18-failure-output.csv") _

    if (args.length == 0) {
      val allOptions = Seq(
        Seq("tms", "greedy"),
//        Seq("tms", "doyle"),
        Seq("tms", "learn")
        //        Seq("clingo", "push")
      )

      val allResults = allOptions map (o => evaluateFailures(o.toArray))

      dump.plotFailures(allResults)

      //      dumpToCsv(allResults)

    } else {
      val results = evaluateFailures(args)
      dump.plotFailures(Seq(results))
      //      dumpToCsv(Seq(results))
    }
  }

  def evaluateFailures(args: Array[String], timePoints: Long = 1000) = {

    val random = new Random(1)

    val evaluationOptions = HashMap(
//      ("P4: 0.01", all_001) -> Seq(P_4),
      ("P4: 0.25", all_025) -> Seq(P_4)
    )

    val evaluationCombination = evaluationOptions map { o =>
      val program = LarsProgram(o._2 flatMap (_.toSeq)) //TODO
      val signals = generateSignals(o._1._2, random, 0, timePoints)

      (o._1._1, program, signals) //TODO
    }

    val option = args.mkString(" ")

    Console.out.println("Algorithm: " + option)

    AlgorithmResult(option, evaluationCombination map (c => executeFailures(args ++ Seq("p18"), c._1, c._2, c._3)) toList) //TODO
  }

  def executeFailures(args: Array[String], instance: String, program: LarsProgram, signals: Seq[StreamEntry]) = {

    Console.out.println(f"Evaluating ${instance}")

    val provider = () => Evaluator.buildEngineFromArguments(args, s => program)

    val e = Evaluator(provider, 1, 2)

    val computations = e.successfulModelComputations(signals)

    SuccessConfigurationResult(instance, computations)
  }


}

trait P18Program {
  def tiDi50(atom: Atom) = WindowAtom(SlidingTimeWindow(50), Diamond, atom)

  def tiBo3(atom: Atom) = WindowAtom(SlidingTimeWindow(3), Box, atom)

  def tuDi50(atom: Atom) = WindowAtom(SlidingTupleWindow(50), Diamond, atom)

  def tuBo3(atom: Atom) = WindowAtom(SlidingTupleWindow(3), Box, atom)

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")

  val e = Atom("e")
  val f = Atom("f")

  val g = Atom("g")
  val h = Atom("h")

  val u = Atom("u")

  // signals
  val x = Atom("x")
  val y = Atom("y")

  // constants
  val i = StringValue("i")
  val j = StringValue("j")

  val a_i = a(i)
  val b_i = b(i)
  val c_i = c(i)
  val d_i = d(i)

  val _1 = StringValue("1")
  val _2 = StringValue("2")
  val _3 = StringValue("3")
  val _4 = StringValue("4")

  val x_1: Atom = x(_1)
  val x_2: Atom = x(_2)
  val x_3: Atom = x(_3)
  val x_4: Atom = x(_4)

  val y_1: Atom = y(_1)
  val y_2: Atom = y(_2)


  val P_1: Seq[LarsRule] = Seq(
    a_i(_1) <= tiDi50(x_1),
    a_i(_2) <= tiDi50(x_2),
    a_i(_3) <= tiDi50(x_3),
    a_i(_4) <= tiDi50(x_4),

    b_i(_1) <= tiBo3(y_1),
    b_i(_2) <= tiBo3(y_1),

    c_i(_1) <= a_i(_1) and a_i(_2) not b_i(_1),
    c_i(_2) <= a_i(_3) and a_i(_4) not b_i(_2),

    d_i(_1) <= c_i(_1),
    d_i(_2) <= c_i(_2)
  )

  val a_j = a(j)
  val b_j = b(j)
  val c_j = c(j)
  val d_j = d(j)

  val P_2: Seq[LarsRule] = Seq(
    a_j(_1) <= tuDi50(x_1),
    a_j(_2) <= tuDi50(x_2),
    a_j(_3) <= tuDi50(x_3),
    a_j(_4) <= tuDi50(x_4),

    b_j(_1) <= tuBo3(y_1),
    b_j(_2) <= tuBo3(y_1),

    c_j(_1) <= a_j(_1) and a_j(_2) not b_j(_1),
    c_j(_2) <= a_j(_3) and a_j(_4) not b_j(_2),

    d_j(_1) <= c_j(_1),
    d_j(_2) <= c_j(_2)
  )

  val P_3: Seq[LarsRule] = P_1 ++ P_2 ++ Seq[LarsRule](
    e(i) <= a_i(_1) and a_i(_2) and a_i(_3) and a_i(_4),
    e(j) <= a_j(_1) and a_j(_2) and a_j(_3) and a_j(_4),

    f(_1) <= e(i) and e(j),
    f(_2) <= e(i) not e(j),
    f(_3) <= e(j) not e(i)
  )

  val P_4: Seq[LarsRule] = P_3 ++ Seq[LarsRule](
    g(i) <= b_i(_1),
    g(i) <= b_i(_2),
    g(j) <= b_j(_1),
    g(j) <= b_j(_2),

    h <= g(i),
    h <= g(j),

    u(_1) <= c_i(_1) and c_j(_2) not h not u(_1),
    u(_2) <= c_i(_2) and c_j(_1) not h not u(_2)
  )

}
