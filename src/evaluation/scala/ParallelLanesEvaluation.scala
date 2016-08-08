import java.io.File

import evaluation.TimedEvaluationEngine
import core.Atom
import core.lars.{Diamond, _}
import evaluation.reachBlocked.ParallelLanes
import evaluation.Evaluator


/**
  * Created by FM on 06.08.16.
  */
object ParallelLanesEvaluation {

  def execute(args: Array[String], nodes: Int, lanes: Int) = {

    val pl = new ParallelLanes {}

    val program = pl.generateProgram(nodes, lanes)

    val provider = () => Evaluator.buildEngineFromArguments(args, s => program)

    val e = Evaluator(provider, 1, 2)

    val obstacles = pl.generatedNodes.map(pl.obstacle(_)).toSet.subsets().toList

    val inputs: Seq[(TimePoint, Seq[Atom])] = obstacles zip (Stream from 1) map (t => ((TimePoint(t._2), t._1.toSeq)))


    val result = e.streamInputsAsFastAsPossible(inputs)

    (f"${nodes}x${lanes}", result)
  }

  def main(args: Array[String]): Unit = {
    var usedArgs = args
    if (usedArgs.length == 0) {
      usedArgs = Seq("tms", "greedy") toArray
    }

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

    val results = evaluationOptions map (o => execute(usedArgs ++ Seq("parallel"), o._1, o._2)) toList

    printToFile(new File("output.csv")) { p =>
      val captions = Seq(
        "Configuration",
        "Append-Min [ms]",
        "Append-Max [ms]",
        "Append-Avg [ms]",
        "Append-Median [ms]",
        "Evaluate-Min [ms]",
        "Evaluate-Max [ms]",
        "Evaluate-Avg [ms]",
        "Evaluate-Median [ms]"
      )

      p.println(captions.mkString(";"))

      val resultStrings = results map (r => Seq(r._1) ++ r._2._1.asResult() ++ r._2._2.asResult())

      resultStrings foreach (r => p.println(r.mkString(";")))
    }

    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try {
        op(p)
      } finally {
        p.close()
      }
    }
  }
}
