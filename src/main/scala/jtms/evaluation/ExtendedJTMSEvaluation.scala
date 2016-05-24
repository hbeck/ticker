package jtms.evaluation

import core.Evaluation
import core.asp.PlainAspProgram
import jtms.ExtendedJTMS

/**
  * Created by hb on 25.03.16.
  */
class ExtendedJTMSEvaluation extends Evaluation {

  def apply(program: PlainAspProgram) = {
    val tmn = ExtendedJTMS(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
