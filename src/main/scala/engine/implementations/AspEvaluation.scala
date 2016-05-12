package engine.implementations

import clingo.ClingoConversion
import core.asp.AspProgram
import engine.{Result, Stream, Time}

import scala.concurrent.duration._

trait EvaluationMode

case class UseFuture(waitingAtMost: Duration = 1 second) extends EvaluationMode

object Direct extends EvaluationMode

trait AspEvaluation {
  def prepare(time: Time, dataStream: Stream): Result
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
