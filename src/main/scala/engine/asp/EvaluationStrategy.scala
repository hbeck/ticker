package engine.asp

import core.asp.AspProgram
import engine.asp.evaluation.{StreamingAspInterpeter}

import scala.concurrent.duration._

/**
  * Created by FM on 13.05.16.
  */
object EvaluationStrategy {

  def pull(program: AspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPullEvaluationEngine(StreamingAspInterpeter.buildTransformation(program, evaluationMode))
  }

  def push(program: AspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPushEvaluationEngine(StreamingAspInterpeter.buildTransformation(program, evaluationMode))
  }

}


trait EvaluationMode

case class UseFuture(waitingAtMost: Duration = 1 second) extends EvaluationMode

object Direct extends EvaluationMode
