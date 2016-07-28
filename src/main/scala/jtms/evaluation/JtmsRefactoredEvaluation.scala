package jtms.evaluation

import core.Evaluation
import core.asp.NormalProgram
import jtms.{JtmsDoyle, JtmsDoyleRefactored$}

/**
  * Created by FM on 25.02.16.
  */
class JtmsRefactoredEvaluation extends Evaluation {

  def apply(program: NormalProgram) = {
    val tmn = JtmsDoyle(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
