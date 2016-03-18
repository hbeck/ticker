package asp

import core.EvaluationResult.Model
import core._

/**
  * Created by FM on 25.02.16.
  */

object Asp {
  def apply(program: Program): Set[Model] = {
    val asp = new Asp(ClingoWrapper())

    asp(program)
  }

  def apply() = new Asp(ClingoWrapper())
}

class Asp(val clingo: ClingoWrapper) extends Evaluation {

  def apply(program: Program): Set[Model] = {
    val expressions = AspConversion(program)

    val result = clingo.run(expressions)
    val models = clingo.parseResult(result)

    if (models.isDefined) {
      val convertedModels = models.get.map(m => m.map(a => Atom(a)))
      return convertedModels
    }

    Set()
  }

}
