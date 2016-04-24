package engine.implementations

import asp.AspConversion
import core.Program
import engine.{Atom, Result, Time}

trait EvaluationMode

object UseFuture extends EvaluationMode

object Direct extends EvaluationMode

trait AspEvaluation {
  def prepare(time: Time, atoms: Set[Atom]): Result
}

object AspEvaluation {

  def pull(program: Program, evaluationMode: EvaluationMode = Direct) = {
    AspPullEvaluation(buildTransformation(program, evaluationMode))
  }

  def buildTransformation(program: Program, evaluationMode: EvaluationMode): AspEvaluation = {
    val transformation = StreamingAspTransformation(AspConversion(program))

    evaluationMode match {
      case UseFuture => FutureAspEvaluation(transformation)
      case _ => transformation
    }
  }
}
