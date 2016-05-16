package engine.asp.evaluation

import engine.asp.now
import core._
import core.lars.TimePoint
import engine.{Result, _}

/**
  * Created by FM on 13.05.16.
  */
trait AspEvaluation extends ((TimePoint, Stream) => Result)


// TODO: naming
case class AspEvaluationEngine(interpreter: StreamingAspInterpeter) extends AspEvaluation {

  // TODO: discuss if only timepoint makes sense here (guess TimeVariable not???)
  def apply(time: TimePoint, dataStream: Stream): Result = {
    val atoms = PinToTimePoint(time)(dataStream)

    val aspResult = interpreter(atoms)

    // TODO: should we also 'unpin' atoms here? (remove (T) ?)
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
  def removeAtoms(timePoint: TimePoint, model: Model): Model = {
    val atoms = model.filterNot {
      case AtomWithArguments(`now`, _) => true
      case AtomWithTime(atom, time) => time != timePoint || atom == now

      case AtomWithArguments(atom, last :: Nil) => last.toLong != timePoint.timePoint


      case _ => false
    }

    atoms
  }
}