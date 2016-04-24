package engine.implementations

import asp.AspConversion
import core.Program
import engine.{Atom, Result, Time}

import scala.concurrent.duration._

trait EvaluationMode

case class UseFuture(waitingAtMost: Duration = 1 second) extends EvaluationMode

object Direct extends EvaluationMode

trait AspEvaluation {
  def prepare(time: Time, atoms: Set[Atom]): Result
}

object AspEvaluation {

  def pull(program: Program, evaluationMode: EvaluationMode = Direct) = {
    AspPullEvaluation(buildTransformation(program, evaluationMode))
  }

  def push(program: Program, evaluationMode: EvaluationMode = Direct) = {
    AspPushEvaluation(buildTransformation(program, evaluationMode))
  }

  def buildTransformation(program: Program, evaluationMode: EvaluationMode): AspEvaluation = {
    val transformation = StreamingAspTransformation(AspConversion(program))

    evaluationMode match {
      case UseFuture(waitingAtMost: Duration) => FutureAspEvaluation(transformation, waitingAtMost)
      case _ => transformation
    }
  }
}
