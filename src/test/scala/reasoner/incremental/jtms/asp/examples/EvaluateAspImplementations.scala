package reasoner.incremental.jtms.asp.examples

import core.Evaluation
import evaluation.iclp.JtmsGreedyEvaluation
import org.scalatest.FlatSpec
import reasoner.asp.clingo.ClingoCall

/**
  * Created by FM on 25.02.16.
  */
trait EvaluateAspImplementations {
  this: FlatSpec =>

  val asp = ClingoCall()
  val answerUpdate = new JtmsGreedyEvaluation

  def theSame(tests: => Evaluation => Unit) = {
    "The ASP implementation" should behave like tests(asp)
    "The AnswerUpdate implementation" should behave like tests(answerUpdate)
  }
}
