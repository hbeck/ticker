package engine.implementations

import clingo.ClingoConversion
import core.asp.AspProgram
import engine.{TimePoint, Result, Stream}

import scala.concurrent.duration._

trait EvaluationMode

case class UseFuture(waitingAtMost: Duration = 1 second) extends EvaluationMode

object Direct extends EvaluationMode

trait AspEvaluation {
  // TODO: discuss if timepoint makes sense here (guess TimeVariable not???)
  def prepare(time: TimePoint, dataStream: Stream): Result
}

object AspEvaluation {

  def pull(program: AspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPullEvaluation(buildTransformation(program, evaluationMode))
  }

  def push(program: AspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPushEvaluation(buildTransformation(program, evaluationMode))
  }

  def buildTransformation(program: AspProgram, evaluationMode: EvaluationMode): AspEvaluation = {
    val transformation = StreamingAspEvaluation(ClingoConversion(program))

    evaluationMode match {
      case UseFuture(waitingAtMost: Duration) => FutureAspEvaluation(transformation, waitingAtMost)
      case _ => transformation
    }
  }
}
