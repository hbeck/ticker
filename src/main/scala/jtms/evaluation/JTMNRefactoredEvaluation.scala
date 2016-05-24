package jtms.evaluation

import core.Evaluation
import core.asp.PlainAspProgram
import jtms.JTMNRefactored

/**
  * Created by FM on 25.02.16.
  */
class JTMNRefactoredEvaluation extends Evaluation {

  def apply(program: PlainAspProgram) = {
    val tmn = JTMNRefactored(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
