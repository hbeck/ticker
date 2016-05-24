package engine.asp.evaluation

import clingo.{ClingoConversion, ClingoProgram, _}
import core.Model
import core.lars.TimePoint

/**
  *
  * Created by FM on 22.04.16.
  */
case class StreamingClingoInterpreter(program: ClingoProgram, clingoEvaluation: ClingoEvaluation = ClingoEvaluation()) extends StreamingAspInterpeter {

  def apply(timePoint: TimePoint, pinnedAtoms: PinnedStream): Option[Model] = {

    val transformed = pinnedAtoms map (ClingoConversion(_))

    val aspResult = clingoEvaluation(program ++ transformed).headOption

    aspResult
  }

}
