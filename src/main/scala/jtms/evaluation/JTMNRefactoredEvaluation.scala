package jtms.evaluation

import core.Evaluation
import core.asp.NormalProgram
import jtms.JtmsRefactored

/**
  * Created by FM on 25.02.16.
  */
class JTMNRefactoredEvaluation extends Evaluation {

  def apply(program: NormalProgram) = {
    val tmn = JtmsRefactored(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
