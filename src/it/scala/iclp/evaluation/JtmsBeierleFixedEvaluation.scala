package iclp.evaluation

import core.Evaluation
import core.asp.NormalProgram
import reasoner.incremental.jtms.algorithms.JtmsBeierleFixed

/**
  * Created by FM on 25.02.16.
  */
class JtmsBeierleFixedEvaluation extends Evaluation {

  def apply(program: NormalProgram) = {
    val tmn = JtmsBeierleFixed(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
