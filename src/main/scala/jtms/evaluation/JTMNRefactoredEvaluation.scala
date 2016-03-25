package jtms.evaluation

import core.{Evaluation, Program}
import jtms.JTMNRefactored

/**
  * Created by FM on 25.02.16.
  */
class JTMNRefactoredEvaluation extends Evaluation {

  def apply(program: Program) = {
    val tmn = JTMNRefactored(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
