package engine.asp.evaluation

import clingo.{ClingoConversion, ClingoExpression, ClingoProgram, _}
import core.lars.TimePoint
import core.{Atom, AtomWithArguments, Model}
import engine._

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

  def apply(pinnedAtoms: Set[PinnedAspRule]): Option[Model] = {

    val transformed = pinnedAtoms map (ClingoConversion(_))

    val aspResult = aspEngine(aspExpressions ++ transformed).headOption

    aspResult
  }
}
