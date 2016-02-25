package jtms

import core.{EvaluationResult, SingleModel, Program, Evaluation}

/**
  * Created by FM on 25.02.16.
  */
class jTmn extends Evaluation {

  def apply(program: Program): Option[EvaluationResult] = {
    val tmn = TMN(program)

    val model = tmn.getModel()

    if (model.nonEmpty)
      return Some(SingleModel(model))

    return None
  }

}
