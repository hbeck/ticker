import java.io.File

import com.sun.org.glassfish.external.statistics.Statistic
import evaluation.{DumpDataToCsv, Evaluator, StatisticResult, TimedEvaluationEngine}
import core.Atom
import core.lars.{Diamond, _}
import evaluation.reachBlocked.ParallelLanes


/**
  * Created by FM on 06.08.16.
  */
object ParallelLanesEvaluation {

  def main(args: Array[String]): Unit = {

    // evaluate everything one time as pre-pre-warmup
    evaluate(Seq("tms", "greedy") toArray) toArray

    val captions = Seq(
      "Configuration",
      "node x lanes"
    )

    val dataDump = DumpDataToCsv.printResults("output.csv") _

    if (args.length == 0) {
      val allOptions = Seq(
        Seq("tms", "greedy"),
        Seq("tms", "doyle"),
        Seq("clingo", "push")
      )

      val allResults = allOptions flatMap (o => evaluate(o.toArray))

      dataDump(captions, allResults)

    } else {
      val results = evaluate(args)

      dataDump(captions, results)
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
      (2, 4)
      //      (3, 3),
      //      (3, 4),
      //      (4, 4)
    )

    evaluationOptions map (o => execute(args ++ Seq("parallel"), o._1, o._2)) toList
  }


  def execute(args: Array[String], nodes: Int, lanes: Int) = {

    val options = Seq(args.mkString(" "), f"${nodes}x${lanes}")

    Console.out.println(f"Evaluating ${options.mkString(" ")}")

    val pl = new ParallelLanes {}
    val program = pl.generateProgram(nodes, lanes)

    val provider = () => Evaluator.buildEngineFromArguments(args, s => program)

    val e = Evaluator(provider, 1, 2)

    val obstacles = pl.generatedNodes.map(pl.obstacle(_)).toSet.subsets().toList
    val inputs: Seq[(TimePoint, Seq[Atom])] = obstacles zip (Stream from 1) map (t => (TimePoint(t._2), t._1.toSeq))

    val result = e.streamInputsAsFastAsPossible(inputs)

    (options, result)
  }


}
