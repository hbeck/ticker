package engine.asp.oneshot

import core._
import core.lars.TimePoint
import engine.asp._
import engine.asp.tms.Pin
import engine.{Result, _}

/**
  * Created by FM on 13.05.16.
  */
trait OneShotEvaluation {
  def apply(timePoint: TimePoint, dataStream: Stream): Result
}

case class OneShotEvaluationEngine(interpreter: StreamingAspInterpreter) extends OneShotEvaluation {

  def apply(time: TimePoint, dataStream: Stream): Result = {
    val input = OneShotEvaluationEngine.pinnedInput(time, dataStream)

    val aspResult = interpreter(time, input)

    val result = aspResult match {
      case Some(model) => Some(PinnedModelToLarsModel(time, model))
      case None => None
    }

    new Result {
      override def get: Option[Model] = result
    }
  }
}

object OneShotEvaluationEngine {

  def pinnedInput(time: TimePoint, dataStream: Stream) = pin(dataStream) + Pin(time)(now)

  def pin(dataStream: Stream): PinnedStream = dataStream flatMap (x => x.atoms map Pin(x.time).apply)

}