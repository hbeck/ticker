package aspsamples

import asp.Asp
import core.Evaluation
import jtms.evaluation.{JTMNBeierleEvaluation, JTMNRefactoredEvaluation}
import org.scalatest.FlatSpec

/**
  * Created by FM on 25.02.16.
  */
trait EvaluateJTMNImplementations {
  this: FlatSpec =>

  val asp = Asp()
  val jtmnBeierle = new JTMNBeierleEvaluation
  val jtmnRefactored = new JTMNRefactoredEvaluation
  //val answerUpdateNetwork = new AnswerUpdateNetworkEvaluation

  def theSame(tests: => Evaluation => Unit) = {
    "The ASP implementation" should behave like tests(asp)
    "The JTMNBeierle implementation" should behave like tests(jtmnBeierle)
    "The JTMNRefactored implementation" should behave like tests(jtmnRefactored)
    //"The AnswerUpdateNetwork implementation" should behave like tests(answerUpdateNetwork)
  }
}
