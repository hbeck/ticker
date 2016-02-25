package asp

import core._

/**
  * Created by FM on 25.02.16.
  */

object Asp {
  def apply(program: Program): Option[EvaluationResult] = {
    val asp = new Asp(ClingoWrapper())

    asp(program)
  }

  def apply() = new Asp(ClingoWrapper())
}

class Asp(val clingo: ClingoWrapper) extends Evaluation {

  def apply(program: Program): Option[EvaluationResult] = {
    val expressions = AspConversion(program)

    val result = clingo.run(expressions)
    val models = clingo.parseResult(result)

    if (models.isDefined) {
      val convertedModels = models.get.map(m => m.map(a => Atom(a)))
      if (convertedModels.size > 1)
        return Some(MultipleModels(convertedModels))
      else
        return Some(SingleModel(convertedModels.head))
    }

    None
  }

}
