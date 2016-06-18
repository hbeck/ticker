package engine.asp.evaluation

import core._
import core.asp.AspFact
import core.lars.TimePoint
import engine.asp._
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
    val input = AspEvaluationEngine.pinnedInput(time, dataStream)

    val aspResult = interpreter(time, input)

    val result = aspResult match {
      case Some(model) => Some(AspEvaluationEngine.translateToLars(time, model))
      case None => None
    }

    new Result {
      override def get: Option[Model] = result
    }
  }
}

object AspEvaluationEngine {

  def pinnedInput(time: TimePoint, dataStream: Stream) = pin(dataStream) + PinToTimePoint(time)(now)

  def pin(dataStream: Stream): PinnedStream = dataStream flatMap (x => PinToTimePoint(x.time).atoms(x.atoms))

  def translateToLars(timePoint: TimePoint, model: PinnedModel): Model = {

    val filtered = model filter {
      case PinnedAtom(`now`, _) => false
      case PinnedAtom(atom, time) => time == timePoint
      case _ => true
    }

    val unpinned = filtered map (_.atom)

    unpinned
  }

}