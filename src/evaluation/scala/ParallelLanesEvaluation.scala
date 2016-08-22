import core.lars._
import engine.StreamEntry
import evaluation._
import evaluation.reachBlocked.ParallelLanes

/**
  * Created by FM on 06.08.16.
  */
object ParallelLanesEvaluation {

  def main(args: Array[String]): Unit = {


    // evaluate everything one time as pre-pre-warmup
    evaluate(Seq("tms", "greedy") toArray)

    val dump = DumpData("Configuration", "node x lanes")
    val dumpToCsv = dump.printResults("output.csv") _

    if (args.length == 0) {
      val allOptions = Seq(
        Seq("tms", "greedy"),
        Seq("tms", "doyle"),
        Seq("tms", "learn")
//        Seq("clingo", "push")
      )

      val allResults = allOptions map (o => evaluate(o.toArray))

      dump.plot(allResults)

      dumpToCsv(allResults)

    } else {
      val results = evaluate(args)
      dump.plot(Seq(results))
      dumpToCsv(Seq(results))
    }
  }


  def evaluate(args: Array[String]) = {
    val evaluationOptions = Seq(
      (1, 1),
      (1, 2),
      (1, 3),
      (1, 4),
      (2, 2),
      (2, 3),
      (2, 4),
      (3, 3),
      (3, 4),
      (4, 4)
    )

    val option = args.mkString(" ")

    AlgorithmResult(option, evaluationOptions map (o => execute(args ++ Seq("parallel"), o._1, o._2)) toList)
  }


  def execute(args: Array[String], nodes: Int, lanes: Int) = {

    val instance = f"${nodes}x${lanes}"


    Console.out.println(f"Evaluating ${instance}")

    val pl = new ParallelLanes {}
    val program = pl.generateProgram(nodes, lanes)

    val provider = () => Evaluator.buildEngineFromArguments(args, s => program)

    val e = Evaluator(provider, 1, 2)

    val obstacles = pl.generatedNodes.map(pl.obstacle(_)).toSet.subsets().toList
    val inputs: Seq[StreamEntry] = obstacles zip (Stream from 1) map (t => StreamEntry(TimePoint(t._2), t._1.toSet))

    val (append, evaluate) = e.streamInputsAsFastAsPossible(inputs)

    TimingsConfigurationResult(instance, append, evaluate)
  }


}
