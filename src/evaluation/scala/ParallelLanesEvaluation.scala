import evaluation.TimedEvaluationEngine

import core.Atom
import core.lars.{Diamond, _}
import evaluation.reachBlocked.ParallelLanes
import evaluation.{Evaluator, Evaluator2}


/**
  * Created by FM on 06.08.16.
  */
object ParallelLanesEvaluation {

  def main(args: Array[String]): Unit = {

    val pl = new ParallelLanes {

    }

    val a = Atom("a")
    val y = Atom("y")


    val program = pl.generateProgram(2, 4)

    val ySample = LarsProgram.from(
      y <= W(1, Diamond, a)
    )

    val provider = () => Evaluator.greedyTms(program)

    val e = Evaluator2(provider, 1, 1)

    val obstacles = pl.generatedNodes.map(pl.obstacle(_)).toSet.subsets().toList


    val inputs: Seq[(TimePoint, Seq[Atom])] = obstacles zip (Stream from 1) map (t => ((TimePoint(t._2), t._1.toSeq)))

    //  val inputs = Seq(
    //    (TimePoint(1), Seq(a)),
    //    (TimePoint(3), Seq(a)),
    //    (TimePoint(5), Seq(a))
    //  )

    val (append, evaluate) = e.streamInputsAsFastAsPossible(inputs)

    Console.out.println(append)
    Console.out.println(evaluate)
  }
}
