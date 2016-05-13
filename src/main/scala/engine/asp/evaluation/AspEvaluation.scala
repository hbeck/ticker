package engine.asp.evaluation

import engine.asp.now
import core.{Atom, AtomWithArguments, Model}
import core.lars.TimePoint
import engine.{Result, _}

/**
  * Created by FM on 13.05.16.
  */
trait AspEvaluationT extends ((TimePoint, Stream) => Result)

case class AspEvaluation(interpreter: StreamingAspInterpeter) extends AspEvaluationT {

  // TODO: discuss if only timepoint makes sense here (guess TimeVariable not???)
  def apply(time: TimePoint, dataStream: Stream): Result = {
    val atoms = PinToTimePoint(time)(dataStream)

    val aspResult = interpreter(atoms)

    val result = aspResult match {
      case Some(model) => Some(removeNow(model))
      case None => None
    }

    new Result {
      override def get: Option[Model] = result
    }
  }

  def removeNow(model: Model): Model = {
    val atoms = model.filterNot {
      case AtomWithArguments(baseAtom, _) => baseAtom == now
      case _ => false
    }

    atoms
  }
}
