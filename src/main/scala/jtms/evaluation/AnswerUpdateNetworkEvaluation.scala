package jtms.evaluation

import core.{Evaluation, Program}
import jtms.{AnswerUpdateNetwork, AnswerUpdateNetwork$}

/**
  * Created by hb on 25.03.16.
  */
class AnswerUpdateNetworkEvaluation extends Evaluation {

  def apply(program: Program) = {
    val tmn = AnswerUpdateNetwork(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
