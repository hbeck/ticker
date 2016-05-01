package jtms.evaluation

import core.Evaluation
import core.asp.AspProgram
import jtms.ExtendedJTMS

/**
  * Created by hb on 25.03.16.
  */
class ExtendedJTMSEvaluation extends Evaluation {

  def apply(program: AspProgram) = {
    val tmn = ExtendedJTMS(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
