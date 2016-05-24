package jtms.evaluation

import core.Evaluation
import core.asp.NormalProgram
import jtms.ExtendedJtms

/**
  * Created by hb on 25.03.16.
  */
class ExtendedJTMSEvaluation extends Evaluation {

  def apply(program: NormalProgram) = {
    val tmn = ExtendedJtms(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
