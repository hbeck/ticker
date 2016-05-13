package engine.asp.evaluation

import clingo.{ClingoExpression, ClingoProgram, _}
import core.lars.TimePoint
import core.{Atom, AtomWithArguments}
import engine._
import engine.asp.now

/**
  *
  * Created by FM on 22.04.16.
  */
object StreamingAspToClingo {
  def apply(time: TimePoint, dataStream: Stream): Set[ClingoExpression] = {
    PinToTimePoint(time)(dataStream) map (ClingoConversion(_))
  }
}

case class StreamingClingoInterpreter(aspExpressions: ClingoProgram, aspEngine: ClingoEvaluation = ClingoEvaluation()) extends StreamingAspInterpeter {

  def prepare(time: TimePoint, dataStream: Stream): Result = {

    val transformed = StreamingAspToClingo(time, dataStream)

    val aspResult = aspEngine(aspExpressions ++ transformed).headOption

    val result = aspResult match {
      case Some(model) => {
        val atoms = model.filterNot {
          case AtomWithArguments(baseAtom, _) => baseAtom == now
          case _ => false
        }
        Some(atoms)
      }
      case None => None
    }

    new Result {
      override def get: Option[Set[Atom]] = result
    }
  }
}
