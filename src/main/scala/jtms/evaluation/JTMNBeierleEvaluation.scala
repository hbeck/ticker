package jtms.evaluation

import core.{Evaluation, Program}
import jtms.JTMNBeierle

/**
  * Created by FM on 25.02.16.
  */
class JTMNBeierleEvaluation extends Evaluation {

  def apply(program: Program) = {
    val tmn = JTMNBeierle(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
