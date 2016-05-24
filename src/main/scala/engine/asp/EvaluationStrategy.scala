package engine.asp

import core.asp.PlainAspProgram
import engine.asp.evaluation._

import scala.concurrent.duration._

/**
  * Created by FM on 13.05.16.
  */
object EvaluationStrategy {

  def pull(program: PlainAspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPullEvaluationEngine(buildTransformation(program, evaluationMode))
  }

  def push(program: PlainAspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPushEvaluationEngine(buildTransformation(program, evaluationMode))
  }

  def buildTransformation(program: PlainAspProgram, evaluationMode: EvaluationMode): AspEvaluation = {
    val evaluation = AspEvaluationEngine(StreamingAspInterpeter.select(program, Clingo))

    evaluationMode match {
      case UseFuture(waitingAtMost: Duration) => FutureStreamingAspInterpeter(evaluation, waitingAtMost)
      case _ => evaluation
    }
  }

}


trait EvaluationMode

case class UseFuture(waitingAtMost: Duration = 1 second) extends EvaluationMode

object Direct extends EvaluationMode
