package jtms.evaluation

import core.Evaluation
import core.asp.NormalProgram
import jtms.JtmsExtended

/**
  * Created by hb on 25.03.16.
  */
class JtmsExtendedEvaluation extends Evaluation {

  def apply(program: NormalProgram) = {
    val tmn = JtmsExtended(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}