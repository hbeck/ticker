package jtms.evaluation

import core.{Evaluation, Program}
import jtms.AnswerUpdate

/**
  * Created by hb on 25.03.16.
  */
class AnswerUpdateEvaluation extends Evaluation {

  def apply(program: Program) = {
    val tmn = AnswerUpdate(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
