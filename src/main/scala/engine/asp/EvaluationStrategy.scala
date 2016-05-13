package engine.asp

import core.asp.AspProgram
import engine.asp.evaluation.AspEvaluation

import scala.concurrent.duration._

/**
  * Created by FM on 13.05.16.
  */
object EvaluationStrategy {

  def pull(program: AspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPullEvaluationEngine(AspEvaluation.buildTransformation(program, evaluationMode))
  }

  def push(program: AspProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPushEvaluationEngine(AspEvaluation.buildTransformation(program, evaluationMode))
  }

}


trait EvaluationMode

case class UseFuture(waitingAtMost: Duration = 1 second) extends EvaluationMode

object Direct extends EvaluationMode
