package engine.asp.oneshot

import core.asp.NormalProgram

import scala.concurrent.duration._

/**
  * Created by FM on 13.05.16.
  */
object EvaluationStrategy {

  def pull(program: NormalProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPullEvaluationEngine(buildTransformation(program, evaluationMode))
  }

  def push(program: NormalProgram, evaluationMode: EvaluationMode = Direct) = {
    AspPushEvaluationEngine(buildTransformation(program, evaluationMode))
  }

  def buildTransformation(program: NormalProgram, evaluationMode: EvaluationMode): AspEvaluation = {
    val evaluation = AspEvaluationEngine(StreamingAspInterpreter.select(program, Clingo))

    evaluationMode match {
      case UseFuture(waitingAtMost: Duration) => FutureStreamingAspInterpreter(evaluation, waitingAtMost)
      case _ => evaluation
    }
  }

}


trait EvaluationMode

case class UseFuture(waitingAtMost: Duration = 1 second) extends EvaluationMode

object Direct extends EvaluationMode
