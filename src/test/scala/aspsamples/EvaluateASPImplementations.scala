package aspsamples

import asp.Asp
import core.Evaluation
import jtms.evaluation.AnswerUpdateNetworkEvaluation
import org.scalatest.FlatSpec

/**
  * Created by FM on 25.02.16.
  */
trait EvaluateASPImplementations {
  this: FlatSpec =>

  val asp = Asp()
  val answerUpdate = new AnswerUpdateNetworkEvaluation

  def theSame(tests: => Evaluation => Unit) = {
    "The ASP implementation" should behave like tests(asp)
    "The AnswerUpdate implementation" should behave like tests(answerUpdate)
  }
}
