package jtms.asp.examples

import clingo.ClingoCall
import core.Evaluation
import iclp.evaluation.JtmsGreedyEvaluation
import org.scalatest.FlatSpec

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
