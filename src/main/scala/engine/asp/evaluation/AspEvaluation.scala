package engine.asp.evaluation

import clingo.ClingoConversion
import core.asp.AspProgram
import core.lars.TimePoint
import engine.asp.{AspPullEvaluationEngine, AspPushEvaluationEngine, EvaluationMode, UseFuture}
import engine.{Result, Stream}

import scala.concurrent.duration._

trait AspEvaluation {
  // TODO: discuss if only timepoint makes sense here (guess TimeVariable not???)
  def prepare(time: TimePoint, dataStream: Stream): Result
}

object AspEvaluation {

  def buildTransformation(program: AspProgram, evaluationMode: EvaluationMode): AspEvaluation = {
    val transformation = StreamingClingoEvaluation(ClingoConversion(program))

    evaluationMode match {
      case UseFuture(waitingAtMost: Duration) => FutureAspEvaluation(transformation, waitingAtMost)
      case _ => transformation
    }
  }
}
