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

  def convertToPinnedAtom(atom: AtomWithArguments) = {
    // TODO: there should be a more elegant way...
    // should probably go to clingo-parser?

    val lastArgument = atom.arguments.last

    val converted = numberFormat.findFirstIn(lastArgument) match {
      case Some(number) => {
        val l = number.toLong

        val atomWithoutTime = atom.arguments.init match {
          case Nil => atom.atom
          case remainingArguments => AtomWithArguments(atom.atom, remainingArguments)
        }

        PinnedAtom(atomWithoutTime, l)
      }
      case _ => atom
    }

    converted
  }

  def removeAtoms(timePoint: TimePoint, model: Model): Model = {
    val convertedAtoms = model map {
      case p: PinnedAtom => p
      case aa: AtomWithArguments => convertToPinnedAtom(aa)
      case a: Atom => a
    }

    val filtered = convertedAtoms filterNot {
      case AtomWithArguments(`now`, _) => true
      case PinnedAtom(atom, time) => time != timePoint || atom == now
      case _ => false
    }

    val unpinned = filtered map {
      case PinnedAtom(a, _) => a
      case a: Atom => a
    }

    unpinned
  }

}