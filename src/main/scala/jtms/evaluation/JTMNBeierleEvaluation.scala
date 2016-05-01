package jtms.evaluation

import core.{Evaluation, AspProgram}
import jtms.JTMNBeierle

/**
  * Created by FM on 25.02.16.
  */
class JTMNBeierleEvaluation extends Evaluation {

  def apply(program: AspProgram) = {
    val tmn = JTMNBeierle(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
