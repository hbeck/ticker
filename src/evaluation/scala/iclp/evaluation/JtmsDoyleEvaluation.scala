package iclp.evaluation

import core.Evaluation
import core.asp.NormalProgram
import jtms.algorithms.JtmsDoyle

/**
  * Created by FM on 25.02.16.
  */
class JtmsDoyleEvaluation extends Evaluation {

  def apply(program: NormalProgram) = {
    val tmn = JtmsDoyle(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
