package engine.asp

import clingo.ClingoConversion
import core.asp.AspProgram
import engine.asp.evaluation._

import scala.concurrent.duration._

/**
  * Created by FM on 13.05.16.
  */
object EvaluationStrategy {

  def pull(program: AspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPullEvaluationEngine(buildTransformation(program, evaluationMode))
  }

  def push(program: AspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPushEvaluationEngine(buildTransformation(program, evaluationMode))
  }

  def buildTransformation(program: AspProgram, evaluationMode: EvaluationMode): AspEvaluationT = {
    val evaluation = AspEvaluation(StreamingAspInterpeter.select(program, Clingo))

    evaluationMode match {
      case UseFuture(waitingAtMost: Duration) => FutureStreamingAspInterpeter(evaluation, waitingAtMost)
      case _ => evaluation
    }
  }

}


trait EvaluationMode

case class UseFuture(waitingAtMost: Duration = 1 second) extends EvaluationMode

object Direct extends EvaluationMode
