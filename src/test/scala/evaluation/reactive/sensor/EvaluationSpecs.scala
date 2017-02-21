package evaluation.reactive.sensor

import core.lars.TimePoint
import engine.StreamEntry
import evaluation.PrepareEvaluator
import org.scalatest.FlatSpec

import scala.util.Random

/**
  * Created by fm on 21/02/2017.
  */
class EvaluationSpecs extends FlatSpec with ExecuteHelper {

  def execute(args: Array[String], windowLength: Long) = {

    val instance = f"$windowLength}"

    val program = timeWindowProgram()(windowLength)

    val evaluator = PrepareEvaluator.fromArguments(args, instance, program)

    val inputs: Seq[StreamEntry] = continuousSignalStream(new Random(1))(1000)

    evaluator.streamAsFastAsPossible(1, 2)(inputs)
  }

}
