package engine.asp.evaluation

import core._
import core.lars.TimePoint
import engine.asp.now
import engine.{Result, _}

/**
  * Created by FM on 13.05.16.
  */
trait AspEvaluation {
  def apply(timePoint: TimePoint, dataStream: Stream): Result
}

// TODO: naming?
case class AspEvaluationEngine(interpreter: StreamingAspInterpreter) extends AspEvaluation {

  def apply(time: TimePoint, dataStream: Stream): Result = {
    val atoms = PinToTimePoint(time)(dataStream)

    val aspResult = interpreter(time, atoms)

    val result = aspResult match {
      case Some(model) => Some(AspEvaluationEngine.removeAtoms(time, model))
      case None => None
    }

    new Result {
      override def get: Option[Model] = result
    }
  }
}

object AspEvaluationEngine {

  def removeAtoms(timePoint: TimePoint, model: PinnedModel): Model = {

    val filtered = model filterNot {
      case PinnedAtom(`now`, _) => true
      case PinnedAtom(atom, time) => time != timePoint
      case _ => false
    }

    val unpinned = filtered map {
      case PinnedAtom(a, _) => a
      case a: Atom => a
    }

    unpinned
  }

}