package jtms.asp.examples

import asp.Asp
import core.Evaluation
import jtms.evaluation.ExtendedJTMSEvaluation
import org.scalatest.FlatSpec

/**
  * Created by FM on 25.02.16.
  */
trait EvaluateASPImplementations {
  this: FlatSpec =>

  val asp = Asp()
  val answerUpdate = new ExtendedJTMSEvaluation

  def theSame(tests: => Evaluation => Unit) = {
    "The ASP implementation" should behave like tests(asp)
    "The AnswerUpdate implementation" should behave like tests(answerUpdate)
  }
}
