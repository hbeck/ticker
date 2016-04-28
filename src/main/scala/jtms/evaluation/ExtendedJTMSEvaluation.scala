package jtms.evaluation

import core.{Evaluation, Program}
import jtms.ExtendedJTMS

/**
  * Created by hb on 25.03.16.
  */
class ExtendedJTMSEvaluation extends Evaluation {

  def apply(program: Program) = {
    val tmn = ExtendedJTMS(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
