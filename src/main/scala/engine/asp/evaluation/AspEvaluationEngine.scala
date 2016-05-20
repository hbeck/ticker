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


// TODO: naming
case class AspEvaluationEngine(interpreter: StreamingAspInterpeter) extends AspEvaluation {

  // TODO: discuss if only timepoint makes sense here (guess TimeVariable not???)
  def apply(time: TimePoint, dataStream: Stream): Result = {
    val atoms = PinToTimePoint(time)(dataStream)

    val aspResult = interpreter(time, atoms)

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
  val numberFormat = """\d+""".r

  def removeAtoms(timePoint: TimePoint, model: Model): Model = {
    val atoms = model.filterNot {
      case PinnedAtom(atom, time) => time != timePoint || atom == now
      case AtomWithArguments(`now`, _) => true

      // TODO: there should be a more elegant way...
      case aa: AtomWithArguments => {
        val lastArgument = aa.arguments.last

        numberFormat.findFirstIn(lastArgument) match {
          case Some(number) => {
            val l = number.toLong
            l != timePoint.timePoint
          }
          case _ => false
        }
      }
      case _ => false
    }

    atoms
  }

}