package engine.asp.evaluation

import clingo.{ClingoConversion, ClingoExpression, ClingoProgram, _}
import core.lars.TimePoint
import core.{Atom, AtomWithArguments, Model}
import engine._

/**
  *
  * Created by FM on 22.04.16.
  */
case class StreamingClingoInterpreter(program: ClingoProgram, clingoEvaluation: ClingoEvaluation = ClingoEvaluation()) extends StreamingAspInterpeter {

  def apply(timePoint: TimePoint, pinnedAtoms: Set[PinnedAspRule]): Option[Model] = {

    val transformed = pinnedAtoms map (ClingoConversion(_))

    val aspResult = clingoEvaluation(program ++ transformed).headOption

    aspResult
  }

}
