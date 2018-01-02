package iclp.evaluation

import core.Evaluation
import core.asp.NormalProgram
import reasoner.incremental.jtms.algorithms.JtmsDoyleHeuristics

/**
  * Created by HB on 7.12.2017 (adapted from JtmsDoyleEvaluation)
  */
class JtmsDoyleHeuristicsEvaluation extends Evaluation {

  def apply(program: NormalProgram) = {
    val jtms = JtmsDoyleHeuristics(program)

    val singleModel = jtms.getModel.get

    Set(singleModel)
  }

}
